package parser

import LoxApp.LoxApp
import tokens.Token

import scala.collection.mutable

case class ResolutionScope(
    resolutionStatusByKey: mutable.Map[String, Boolean]
)

object ResolutionScopeOps {
  extension (resolutionScopes: mutable.Stack[ResolutionScope]) {
    def beginScope(): Unit =
      resolutionScopes.push(ResolutionScope(mutable.Map.empty))

    def endScope(): Unit = resolutionScopes.pop()

    // declaration and definition are split into two different steps mainly to
    // handle edge cases where initializer for the variable holds a reference to the variable
    // only after we resolve the initializer expr (and make sure its' free from bugs do we actually declare it)
    def declare(token: Token): Unit = {
      resolutionScopes.headOption match {
        case Some(scope) =>
          if (scope.resolutionStatusByKey.contains(token.lexeme)) {
            LoxApp.error(
              token,
              "Already a variable with this name defined in scope"
            )
          }

          scope.resolutionStatusByKey.addOne((token.lexeme, false))
        case _ =>
      }
    }

    def define(token: Token): Unit = {
      resolutionScopes.headOption match {
        case Some(scope) =>
          scope.resolutionStatusByKey.addOne((token.lexeme, true))
        case _ =>
      }
    }
  }
}
