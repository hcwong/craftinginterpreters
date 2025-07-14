package parser

import tokens.TokenType

sealed trait Expr

/** I dislike the Visitor Pattern I implement expressions as an ADT
  */
object Expr {
  sealed trait Literal extends Expr
  private[Expr] case object TrueLiteral extends Literal
  private[Expr] case object FalseLiteral extends Literal
  private[Expr] case class StringLiteral(value: String) extends Literal
  private[Expr] case class IntLiteral(value: Int) extends Literal
  private[Expr] case object NullLiteral extends Literal

  object Literal {
    def apply(value: true) = TrueLiteral
    def apply(value: false) = FalseLiteral
    def apply(value: String) = StringLiteral(value)
    def apply(value: Int) = IntLiteral(value)
    def apply(value: Null) = NullLiteral
  }

  // TODO: Narrow down the type of operator. Not all tokens are operators
  case class Unary(operator: TokenType, expr: Expr) extends Expr
  case class Binary(leftExpr: Expr, operator: TokenType, rightExpr: Expr)
      extends Expr
  case class Ternary(condition: Expr, positive: Expr, negative: Expr)
      extends Expr
  case class Grouping(expr: Expr) extends Expr

  // TODO: Use Scala 3 typeclasses to implement implicit resolution
  extension (expr: Expr) {
    def print: String = expr match {
      case TrueLiteral           => s"( Literal: true )"
      case FalseLiteral          => s"( Literal: false )"
      case StringLiteral(value)  => s"( Literal: $value )"
      case IntLiteral(value)     => s"( Literal: $value )"
      case NullLiteral           => s"( Null Literal )"
      case Unary(operator, expr) => s"( Unary: $operator ${expr.print} )"
      case Binary(left, op, right) =>
        s"( Binary: ${left.print}, $op, ${right.print} )"
      case Ternary(condition, positive, negative) =>
        s"( Ternary: (${condition.print} ? ${positive.print} : ${negative.print}) )"
      case Grouping(expr) => s"( Grouping: ${expr.print} )"
    }
  }
}
