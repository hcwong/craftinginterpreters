package parser

import tokens.TokenType

sealed trait Expr

// Dislike the visitor pattern
object Expr {
  private sealed trait Primitive
  case class TruePrimitive(value: true) extends Primitive
  case class FalsePrimitive(value: false) extends Primitive
  case class StringPrimitive(value: String) extends Primitive with AnyVal
  case class IntPrimitive(value: Int) extends Primitive with AnyVal
  case class NullPrimitive(value: null) extends Primitive

  case class Literal(value: Primitive) extends Expr
  object Literal {
    def apply(value: true) = TruePrimitive(value)
    def apply(value: false) = FalsePrimitive(value)
    def apply(value: String) = StringPrimitive(value)
    def apply(value: Int) = IntPrimitive(value)
    def apply(value: null) = NullPrimitive(value)
  }

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
