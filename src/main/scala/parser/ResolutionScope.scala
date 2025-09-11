package parser

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

    def declare(token: Token): Unit = {
      resolutionScopes.headOption match {
        case Some(scope) =>
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
