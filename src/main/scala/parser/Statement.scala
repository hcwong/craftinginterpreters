package parser

import tokens.Token

sealed trait Statement
case class VariableDeclaration(identifier: Token, exprOpt: Option[Expr])
    extends Statement

object Statement {

  // Print statements aren't usually part of the compiler, implemented by libs
  case class PrintStatement(expr: Expr) extends Statement
  case class ExprStatement(expr: Expr) extends Statement

  extension (declaration: Statement)(using environment: Environment) {
    def execute(): Unit = {
      declaration match {
        case PrintStatement(expr) =>
          println(expr.evaluate)
        case ExprStatement(expr) => expr.evaluate
        case VariableDeclaration(identifier, exprOpt) =>
          val value = exprOpt
            .map(expr => expr.evaluate)
            .getOrElse(None)
          environment.define(identifier.lexeme, value)
      }
      ()
    }
  }
}
