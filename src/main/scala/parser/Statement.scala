package parser

import tokens.Token
import Expr.isTruthy
import runtime.{FunctionCallable, LoxKlass, ReturnException}
import LoxApp.Constants

import scala.collection.mutable

sealed trait Statement

object Statement {
  case class VariableDeclaration(identifier: Token, exprOpt: Option[Expr])
      extends Statement
  // Print statements aren't usually part of the compiler, implemented by libs
  case class PrintStatement(expr: Expr) extends Statement
  case class ExprStatement(expr: Expr) extends Statement
  case class BlockStatement(statements: Seq[Statement]) extends Statement
  case class IfStatement(
      conditional: Expr,
      ifClause: Statement,
      elseClause: Option[Statement]
  ) extends Statement
  case class WhileStatement(
      condition: Expr,
      statement: Statement
  ) extends Statement

  case class FunctionDeclaration(
      name: Token,
      parameters: Seq[Token],
      body: Seq[Statement]
  ) extends Statement

  case class ReturnStatement(
      returnKeyword: Token,
      exprToReturn: Option[Expr]
  ) extends Statement

  case class ClassDeclaration(
      name: Token,
      superclass: Option[Expr.Variable],
      methods: Seq[FunctionDeclaration]
  ) extends Statement

  extension (declaration: Statement) {
    def execute(
        environment: Environment,
        locals: mutable.Map[Expr, Int]
    ): Unit = {
      given Environment = environment
      given mutable.Map[Expr, Int] = locals
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
          statements.foreach(_.execute(blockEnv, locals))
        case IfStatement(conditional, ifClause, _)
            if isTruthy(conditional.evaluate) =>
          ifClause.execute(environment, locals)
        case IfStatement(_, _, exprClause) =>
          exprClause.foreach(_.execute(environment, locals))
        case WhileStatement(conditional, bodyStatement) =>
          while (isTruthy(conditional.evaluate)) {
            bodyStatement.execute(environment, locals)
          }
        case fnDeclaration: FunctionDeclaration =>
          val callable =
            FunctionCallable(environment, locals, fnDeclaration, false)
          environment.define(
            fnDeclaration.name.lexeme,
            callable
          )
        case ReturnStatement(_, returnExpr) =>
          throw ReturnException(returnValue = returnExpr match {
            case Some(value) => value.evaluate
            case _           => Expr.Literal(null).evaluate
          })
        case ClassDeclaration(name, superclass, functionDeclarations) =>
          val superklass = superclass
            .map(expr => (expr.evaluate, expr))
            .map { (evaluated, expr) =>
              evaluated match
                case klass: LoxKlass => klass
                case _ =>
                  throw RuntimeError(
                    expr.variableToken,
                    "Superclass has to be a class"
                  )
            }

          // The book first defines and then assign, but I think that's redundant?
          val methodsMap = functionDeclarations
            .map(functionDeclaration =>
              (
                functionDeclaration.name.lexeme,
                FunctionCallable(
                  environment,
                  locals,
                  functionDeclaration,
                  functionDeclaration.name.lexeme == Constants.INIT_FUNCTION
                )
              )
            )
            .toMap
          val klass = LoxKlass(name.lexeme, superklass, methodsMap)
          environment.define(name.lexeme, klass)
      }
      ()
    }
  }
}
