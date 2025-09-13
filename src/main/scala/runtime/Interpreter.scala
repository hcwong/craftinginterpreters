package runtime

import parser.{Environment, Expr, ResolutionScope, RuntimeError, Statement}
import tokens.Token
import LoxApp.LoxApp

import scala.collection.mutable

class Interpreter(private val environment: Environment = Environment.global) {
  private enum FunctionType {
    case NONE, FUNCTION, METHOD
  }

  private val resolutionScope: mutable.Stack[ResolutionScope] =
    mutable.Stack.empty
  private val locals: mutable.Map[Expr, Int] = mutable.Map.empty
  private var currentFunctionType = FunctionType.NONE

  def resolve(statement: Statement): Unit = {
    import ResolutionOps._

    statement.resolve(resolutionScope)
  }

  def interpret(statement: Statement): Unit =
    try {
      statement.execute(environment, locals)
    } catch {
      case e: RuntimeError => LoxApp.runtimeError(e)
      case _               => // let it pass
    }

  // Unused for reference only
//  def interpretBlock(statements: Seq[Statement], env: Environment): Unit = {
//    statements.foreach(stmt =>
//      try {
//        stmt.execute(env, locals)
//      } catch {
//        case e: RuntimeError => LoxApp.runtimeError(e)
//        case _               => // let it pass
//      }
//    )
//  }

  private def resolve(expr: Expr, depth: Int): Unit = locals(expr) = depth

  private def resolveLocal(
      resolutionScopes: mutable.Stack[ResolutionScope],
      expr: Expr,
      name: Token
  ): Unit = {
    // iterates from the top of the stack
    // it then finds the first scope that has the name and then resolves the distance,
    // conveniently given by i
    resolutionScopes.zipWithIndex
      .find { case (scope, idx) =>
        scope.resolutionStatusByKey.contains(name.lexeme)
      }
      .foreach { case (_, i) =>
        resolve(expr, i)
      }
  }

  private object ResolutionOps {
    import parser.ResolutionScopeOps._

    private def resolveFunction(
        resolutionScopes: mutable.Stack[ResolutionScope],
        functionDeclaration: Statement.FunctionDeclaration,
        functionType: FunctionType
    ): Unit = {
      val enclosingFunctionType = functionType
      currentFunctionType = functionType
      resolutionScopes.beginScope()
      functionDeclaration.parameters.foreach(param => {
        resolutionScopes.define(param)
        resolutionScopes.declare(param)
      })
      functionDeclaration.body.foreach(_.resolve(resolutionScopes))
      resolutionScopes.endScope()
      currentFunctionType = enclosingFunctionType
    }

    extension (declaration: Statement) {
      def resolve(
          resolutionScopes: mutable.Stack[ResolutionScope]
      ): Unit = {

        declaration match {
          case blockStatement: Statement.BlockStatement =>
            resolutionScopes.beginScope()
            blockStatement.statements.foreach(_.resolve(resolutionScopes))
            resolutionScopes.endScope()
          case variableDeclaration: Statement.VariableDeclaration =>
            resolutionScopes.declare(variableDeclaration.identifier)
            variableDeclaration.exprOpt.foreach(_.resolve(resolutionScopes))
            resolutionScopes.define(variableDeclaration.identifier)
          case functionDeclaration: Statement.FunctionDeclaration =>
            resolutionScopes.declare(functionDeclaration.name)
            resolutionScopes.define(functionDeclaration.name)
            resolveFunction(
              resolutionScopes,
              functionDeclaration,
              FunctionType.FUNCTION
            )
          case exprStatement: Statement.ExprStatement =>
            exprStatement.expr.resolve(resolutionScopes)
          case returnStatement: Statement.ReturnStatement =>
            if (currentFunctionType == FunctionType.NONE) {
              LoxApp.error(
                returnStatement.returnKeyword,
                "Can't return from top level code"
              )
            }

            returnStatement.exprToReturn.foreach(_.resolve(resolutionScopes))
          case ifStatement: Statement.IfStatement =>
            ifStatement.conditional.resolve(resolutionScopes)
            ifStatement.ifClause.resolve(resolutionScopes)
            ifStatement.elseClause.foreach(_.resolve(resolutionScopes))
          case printStatement: Statement.PrintStatement =>
            printStatement.expr.resolve(resolutionScopes)
          case whileStatement: Statement.WhileStatement =>
            whileStatement.condition.resolve(resolutionScopes)
            whileStatement.statement.resolve(resolutionScopes)
          case Statement.ClassDeclaration(name, methods) =>
            resolutionScopes.declare(name)
            resolutionScopes.define(name)
            methods.foreach(
              resolveFunction(resolutionScopes, _, FunctionType.METHOD)
            )
        }
      }
    }

    extension (expr: Expr) {
      private def resolve(
          resolutionScopes: mutable.Stack[ResolutionScope]
      ): Unit =
        expr match {
          case variable: Expr.Variable =>
            if resolutionScopes.nonEmpty && resolutionScopes.top.resolutionStatusByKey
                .get(variable.variableToken.lexeme)
                .contains(false)
            then {
              LoxApp.error(
                variable.variableToken,
                "Can't read local variable in its own initializer"
              )
            } else
              resolveLocal(resolutionScopes, variable, variable.variableToken)
          case assignment: Expr.Assignment =>
            assignment.expr.resolve(resolutionScopes)
            resolveLocal(
              resolutionScopes,
              assignment,
              assignment.identifierToken
            )
          case binaryExpr: Expr.Binary =>
            binaryExpr.leftExpr.resolve(resolutionScopes)
            binaryExpr.rightExpr.resolve(resolutionScopes)
          case callExpr: Expr.Call =>
            callExpr.callee.resolve(resolutionScopes)
            callExpr.arguments.foreach(_.resolve(resolutionScopes))
          case groupingExpr: Expr.Grouping =>
            groupingExpr.expr.resolve(resolutionScopes)
          case logicalExpr: Expr.Logical =>
            logicalExpr.expr.resolve(resolutionScopes)
            logicalExpr.subsequentExpr.resolve(resolutionScopes)
          case ternaryExpr: Expr.Ternary =>
            ternaryExpr.condition.resolve(resolutionScopes)
            ternaryExpr.positive.resolve(resolutionScopes)
            ternaryExpr.negative.resolve(resolutionScopes)
          case unaryExpr: Expr.Unary =>
            unaryExpr.expr.resolve(resolutionScopes)
          case literal: Expr.Literal => ()
          case Expr.Get(callee, _) =>
            callee.resolve(resolutionScopes)
          case Expr.Set(calleeExpr, _, assignmentExpr) =>
            assignmentExpr.resolve(resolutionScopes)
            calleeExpr.resolve(resolutionScopes)
        }
    }
  }
}
