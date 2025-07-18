package parser

import tokens.{Token, TokenType}

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
  case class Unary(operator: Token, expr: Expr) extends Expr {
    def evaluateUnary: Any = {
      val exprEvaluated = expr.evaluate

      operator.tokenType match {
        case TokenType.BANG => !isTruthy(exprEvaluated)
        case TokenType.MINUS =>
          exprEvaluated match {
            case int: Int => -int
            case _ =>
              throw RuntimeError(
                operator,
                s"Attempted to apply token type '-' on non number value $exprEvaluated"
              )
          }
        case _ =>
          throw RuntimeError(
            operator,
            s"Unexpected operator type '${operator.tokenType}' while evaluating unary"
          )
      }
    }
  }

  case class Binary(leftExpr: Expr, operator: Token, rightExpr: Expr)
      extends Expr {
    def evaluateBinary: Any = {
      val leftExprEvaluated = leftExpr.evaluate
      val rightExprEvaluated = rightExpr.evaluate

      operator.tokenType match {
        case TokenType.PLUS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int)       => leftInt + rightInt
            case (leftStr: String, rightStr: String) => leftStr + rightStr
            case _ =>
              throw RuntimeError(
                operator,
                s"'+' cannot be used to add $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.MINUS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) => leftInt - rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'-' cannot be used to subtract $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.STAR =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) => leftInt * rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'*' cannot be used to multiply $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.SLASH =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) if rightInt != 0 =>
              leftInt / rightInt
            case (_, 0) => RuntimeError(operator, "Cannot divide by 0")
            case _ =>
              throw RuntimeError(
                operator,
                s"'/' cannot be used to divide $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.GREATER =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) =>
              leftInt > rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'>' cannot be used to check if $leftExprEvaluated > $rightExprEvaluated"
              )
          }
        case TokenType.GREATER_EQUAL =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) =>
              leftInt >= rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'>=' cannot be used to check if $leftExprEvaluated >= $rightExprEvaluated"
              )
          }
        case TokenType.LESS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) =>
              leftInt < rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'<' cannot be used to check if $leftExprEvaluated < $rightExprEvaluated"
              )
          }
        case TokenType.LESS_EQUAL =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftInt: Int, rightInt: Int) =>
              leftInt <= rightInt
            case _ =>
              throw RuntimeError(
                operator,
                s"'<=' cannot be used to check if $leftExprEvaluated <= $rightExprEvaluated"
              )
          }
        case TokenType.BANG_EQUAL => leftExprEvaluated != rightExprEvaluated
        case TokenType.EQUAL      => leftExprEvaluated == rightExprEvaluated
        case _ =>
          throw RuntimeError(
            operator,
            s"Unexpected operator ${operator.tokenType} in Binary expr"
          )
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
      case binary: Binary       => binary.evaluateBinary
      case Ternary(condition, positive, negative) =>
        if (isTruthy(condition.evaluate)) { positive.evaluate }
        else { negative.evaluate }
      case Grouping(expr) => expr.evaluate
    }
  }
}

case class RuntimeError(token: Token, message: String) extends RuntimeException
