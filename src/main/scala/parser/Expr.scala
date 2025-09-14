package parser

import runtime.{LoxCallable, LoxInstance, LoxKlass}
import tokens.{Token, TokenType}

import scala.collection.mutable

sealed trait Expr

/** I dislike the Visitor Pattern I implement expressions as an ADT
  */
object Expr {
  sealed trait Literal extends Expr
  private[Expr] case object TrueLiteral extends Literal
  private[Expr] case object FalseLiteral extends Literal
  private[Expr] case class StringLiteral(value: String) extends Literal
  private[Expr] case class DoubleLiteral(value: Double) extends Literal
  private[Expr] case object NullLiteral extends Literal
  case class Variable(variableToken: Token) extends Literal

  object Literal {
    def apply(value: true) = TrueLiteral
    def apply(value: false) = FalseLiteral
    def apply(value: String) = StringLiteral(value)
    def apply(value: Double) = DoubleLiteral(value)
    def apply(value: Null) = NullLiteral
  }

  def isTruthy(obj: Any): Boolean = {
    obj match {
      case bool: Boolean => bool
      case None          => false
      case _             => true
    }
  }

  // TODO: Narrow down the type of operator. Not all tokens are operators
  case class Unary(operator: Token, expr: Expr) extends Expr {
    def evaluateUnary(using
        environment: Environment,
        locals: mutable.Map[Expr, Int]
    ) = {
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
    def evaluateBinary(using
        environment: Environment,
        locals: mutable.Map[Expr, Int]
    ) = {
      val leftExprEvaluated = leftExpr.evaluate
      val rightExprEvaluated = rightExpr.evaluate

      operator.tokenType match {
        case TokenType.PLUS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble + rightDouble
            case (leftStr: String, rightStr: String) => leftStr + rightStr
            // Allow addition of String to Double and always return String
            case (leftStr: String, rightDouble: Double) =>
              leftStr + rightDouble.toString
            case (leftDouble: Double, rightString: String) =>
              leftDouble.toString + rightString
            case _ =>
              throw RuntimeError(
                operator,
                s"'+' cannot be used to add $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.MINUS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble - rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'-' cannot be used to subtract $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.STAR =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble * rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'*' cannot be used to multiply $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.SLASH =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double)
                if rightDouble != 0 =>
              leftDouble / rightDouble
            case (_, 0) => RuntimeError(operator, "Cannot divide by 0")
            case _ =>
              throw RuntimeError(
                operator,
                s"'/' cannot be used to divide $leftExprEvaluated and $rightExprEvaluated"
              )
          }
        case TokenType.GREATER =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble > rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'>' cannot be used to check if $leftExprEvaluated > $rightExprEvaluated"
              )
          }
        case TokenType.GREATER_EQUAL =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble >= rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'>=' cannot be used to check if $leftExprEvaluated >= $rightExprEvaluated"
              )
          }
        case TokenType.LESS =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble < rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'<' cannot be used to check if $leftExprEvaluated < $rightExprEvaluated"
              )
          }
        case TokenType.LESS_EQUAL =>
          (leftExprEvaluated, rightExprEvaluated) match {
            case (leftDouble: Double, rightDouble: Double) =>
              leftDouble <= rightDouble
            case _ =>
              throw RuntimeError(
                operator,
                s"'<=' cannot be used to check if $leftExprEvaluated <= $rightExprEvaluated"
              )
          }
        case TokenType.BANG_EQUAL  => leftExprEvaluated != rightExprEvaluated
        case TokenType.EQUAL_EQUAL => leftExprEvaluated == rightExprEvaluated
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
  // Take this for example a = 1 is valid but (a) is not valid
  // In this case, preserving the parentheses aids the accuracy of the interpreter
  case class Grouping(expr: Expr) extends Expr

  case class Assignment(identifierToken: Token, expr: Expr) extends Expr

  enum LogicalOperator {
    case Or, And
  }
  case class Logical(
      expr: Expr,
      operator: LogicalOperator,
      subsequentExpr: Expr
  ) extends Expr

  case class Call(
      callee: Expr,
      closingParen: Token,
      arguments: Seq[Expr]
  ) extends Expr

  case class Get(expr: Expr, propertyName: Token) extends Expr

  case class Set(exprCallee: Expr, propertyName: Token, assignmentValue: Expr)
      extends Expr

  case class This(keyword: Token) extends Expr

  case class Super(keyword: Token, method: Token) extends Expr

  extension (expr: Expr) {
    def print: String = expr match {
      case TrueLiteral             => s"( Literal: true )"
      case FalseLiteral            => s"( Literal: false )"
      case StringLiteral(value)    => s"( Literal: $value )"
      case DoubleLiteral(value)    => s"( Literal: $value )"
      case NullLiteral             => s"( Null Literal )"
      case Variable(variableToken) => s"( Variable: ${variableToken.lexeme} )"
      case Unary(operator, expr)   => s"( Unary: $operator ${expr.print} )"
      case Binary(left, op, right) =>
        s"( Binary: ${left.print}, $op, ${right.print} )"
      case Ternary(condition, positive, negative) =>
        s"( Ternary: (${condition.print} ? ${positive.print} : ${negative.print}) )"
      case Grouping(expr) => s"( Grouping: ${expr.print} )"
      case Assignment(identifierToken, expr) =>
        s"( Assignment of ${identifierToken.lexeme} to expression ${expr.print} )"
      case Logical(expr, operator, subsequentExpr) =>
        s"( Logical: ${expr.print}, $operator.toString, ${subsequentExpr.print}"
      case Call(callee, closingParen, arguments) =>
        s"( Call: ${callee.print}, ${closingParen.toString} with arguments: ${arguments.map(_.print).mkString(", ")}"
      case Get(callee, propertyName) =>
        s"( Get: ${callee.print} with property: $propertyName"
      case Set(callee, propertyName, assignmentValue) =>
        s"( Set: ${callee.print} with property: $propertyName to new value ${assignmentValue.print} )"
      case This(keyword) => s"( This: keyword is token ${keyword} )"
      case Super(keyword, method) =>
        s"( Super: ${keyword} with method ${method} )"
    }

    def evaluate(using
        environment: Environment,
        locals: mutable.Map[Expr, Int]
    ): Any = expr match {
      case TrueLiteral          => true
      case FalseLiteral         => false
      case StringLiteral(value) => value
      case DoubleLiteral(value) => value
      case NullLiteral          => None
      case varExpr: Variable =>
        locals.get(varExpr) match {
          case Some(depth) =>
            environment.getAt(varExpr.variableToken, depth)
          case None =>
            Environment.global.get(varExpr.variableToken)
        }
      case unary: Unary   => unary.evaluateUnary
      case binary: Binary => binary.evaluateBinary
      case Ternary(condition, positive, negative) =>
        if (isTruthy(condition.evaluate)) { positive.evaluate }
        else { negative.evaluate }
      case Grouping(expr) => expr.evaluate
      case assignmentExpr: Assignment =>
        locals.get(assignmentExpr) match {
          case Some(depth) =>
            environment.assignAt(
              assignmentExpr.identifierToken.lexeme,
              assignmentExpr.expr.evaluate,
              depth
            )
          case None =>
            Environment.global.assign(
              assignmentExpr.identifierToken,
              assignmentExpr.expr.evaluate
            )
        }
      case Logical(expr, LogicalOperator.Or, subsequentExpr) =>
        if isTruthy(expr.evaluate) then expr.evaluate
        else subsequentExpr.evaluate
      case Logical(expr, LogicalOperator.And, subsequentExpr) =>
        if !isTruthy(expr.evaluate) then expr.evaluate
        else subsequentExpr.evaluate
      case Call(callee, closingParen, arguments) =>
        callee.evaluate match {
          case loxCallable: LoxCallable =>
            loxCallable.call(arguments.map(_.evaluate))
          case _ =>
            throw RuntimeError(
              closingParen,
              s"${callee} was not of type LoxCallable"
            )
        }
      case Get(callee, propertyName) =>
        callee.evaluate match {
          case loxInstance: LoxInstance =>
            loxInstance.get(propertyName)
          case _ =>
            throw RuntimeError(propertyName, "Only instances have properties")
        }
      case Set(callee, propertyName, assignmentValue) =>
        callee.evaluate match {
          case loxInstance: LoxInstance =>
            loxInstance.set(propertyName, assignmentValue.evaluate)
          case _ =>
            throw RuntimeError(propertyName, "Can only set on instances")
        }
      case thisExpr: This =>
        environment.getAt(
          thisExpr.keyword,
          locals
            .getOrElse(
              thisExpr,
              throw RuntimeError(thisExpr.keyword, "could not resolve this")
            )
        )
      case superExpr: Super =>
        val superklass = environment.getAt(
          superExpr.keyword,
          locals
            .getOrElse(
              superExpr,
              throw RuntimeError(superExpr.keyword, "could not resolve super")
            )
        )
        superklass match {
          case klass: LoxKlass =>
            val loxInstance = environment.getAt(
              "this",
              locals
                .getOrElse(
                  superExpr,
                  throw RuntimeError(
                    superExpr.keyword,
                    "could not resolve super"
                  )
                ) - 1
            )

            loxInstance match {
              case instance: LoxInstance =>
                klass.findMethod(superExpr.method.lexeme) match {
                  case Some(method) => method.bind(instance)
                  case None =>
                    throw RuntimeError(
                      superExpr.method,
                      "method does not exist on superclass"
                    )
                }
              case _ =>
                throw RuntimeError(
                  superExpr.keyword,
                  "Trouble resolving 'this' when calling super"
                )
            }
          case _ =>
            throw RuntimeError(
              superExpr.keyword,
              "super is not a class instance"
            )
        }
    }
  }
}
