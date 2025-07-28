package parser

import tokens.Token
import Expr.isTruthy

sealed trait Statement
case class VariableDeclaration(identifier: Token, exprOpt: Option[Expr])
    extends Statement

object Statement {

  // Print statements aren't usually part of the compiler, implemented by libs
  case class PrintStatement(expr: Expr) extends Statement
  case class ExprStatement(expr: Expr) extends Statement
  case class BlockStatement(statements: Seq[Statement]) extends Statement
  case class IfStatement(
      conditional: Expr,
      ifClause: Expr,
      elseClause: Option[Expr]
  ) extends Statement

  extension (declaration: Statement) {
    def execute(environment: Environment): Unit = {
      given Environment = environment
      declaration match {
        case PrintStatement(expr) =>
          println(expr.evaluate)
        case ExprStatement(expr) => expr.evaluate
        case VariableDeclaration(identifier, exprOpt) =>
          val value = exprOpt
            .map(expr => expr.evaluate)
            .getOrElse(None)
          environment.define(identifier.lexeme, value)
        case BlockStatement(statements) =>
          val blockEnv = Environment(enclosing = environment)
          statements.foreach(_.execute(environment))
        case IfStatement(conditional, ifClause, _)
            if isTruthy(conditional.evaluate) =>
          ifClause.evaluate
        case IfStatement(_, _, exprClause) => exprClause.foreach(_.evaluate)
      }
      ()
    }
  }
}
