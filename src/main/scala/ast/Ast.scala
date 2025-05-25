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

  // TODO: Use Scala 3 typeclasses to implement implicit resolution
  extension(expr: Expr) {
    def print: String = expr match {
      case Literal(value)        => s"( Literal: $value )"
      case Unary(operator, expr) => s"( Unary: $operator ${expr.print} )"
      case Binary(left, op, right) =>
        s"( Binary: ${left.print}, $op, ${right.print} )"
      case Grouping(expr) => s"( Grouping: ${expr.print} )"
    }
  }
}
