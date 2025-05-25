package ast

import tokens.TokenType

// Dislike the visitor pattern
object Ast {
  sealed trait Expr

  case class Literal(value: AnyVal) extends Expr
  // TODO: Narrow down the type of operator. Not all tokens are operators
  case class Unary(operator: TokenType, expr: Expr) extends Expr
  case class Binary(leftExpr: Expr, operator: TokenType, rightExpr: Expr)
      extends Expr
  case class Grouping(expr: Expr) extends Expr

  implicit class ExprPrinter(expr: Expr) {
    def toString: String = expr match {
      case Literal(value)        => s"( Literal: ${value.toString} )"
      case Unary(operator, expr) => s"( Unary: $operator $expr )"
      case Binary(leftExpr, operator, rightExpr) =>
        s"( Binary: $leftExpr, $operator, $rightExpr )"
      case Grouping(expr) => s"( Grouping: $expr )"
    }
  }
}
