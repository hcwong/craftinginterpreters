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

  private def isTruthy(obj: Any): Boolean = {
    obj match {
      case bool: Boolean => bool
      case None          => false
      case _             => true
    }
  }

  // TODO: Narrow down the type of operator. Not all tokens are operators
  case class Unary(operator: TokenType, expr: Expr) extends Expr {
    def evaluateUnary: Any = {
      val exprEvaluated = expr.evaluate

      operator match {
        case TokenType.BANG => !(isTruthy(exprEvaluated))
        case TokenType.MINUS =>
          exprEvaluated match {
            case int: Int => -int
            case _ =>
              sys.error(
                s"Attempted to apply token type '-' on non number value $exprEvaluated"
              )
          }
        case tokenType =>
          sys.error(
            s"Unexpected operator type '$tokenType' while evaluating unary"
          )
      }
    }
  }

  case class Binary(leftExpr: Expr, operator: TokenType, rightExpr: Expr)
      extends Expr {
    def evaluateBinary: Any = {
      val leftExprEvaluated = leftExpr.evaluate
      val rightExprEvaluated = rightExpr.evaluate
      
      operator match {
        case TokenType.
      }
    }
  }
  
  case class Ternary(condition: Expr, positive: Expr, negative: Expr)
      extends Expr
  // Some parsers don't use separate Exprs for Parentheses, but Lox uses it to accurately
  // evaluate LHS of assignment expressions
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

    // Fun idea: Try to make it somehow more typed? No to dynamically typed languages
    def evaluate: Any = expr match {
      case TrueLiteral          => true
      case FalseLiteral         => false
      case StringLiteral(value) => value
      case IntLiteral(value)    => value
      case NullLiteral          => None
      case unary: Unary         => unary.evaluateUnary
    }
  }
}
