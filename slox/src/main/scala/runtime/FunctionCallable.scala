package runtime

import parser.{Environment, Expr}
import parser.Statement.FunctionDeclaration

import scala.collection.mutable

case class FunctionCallable(
    environment: Environment,
    locals: mutable.Map[Expr, Int],
    private val functionDeclaration: FunctionDeclaration,
    private val isInitializer: Boolean
) extends LoxCallable {
  override val arity: Int = functionDeclaration.parameters.size

  override def call(arguments: Seq[Any]): Any = {
    val functionEnvironment = Environment(environment)

    functionDeclaration.parameters.zipWithIndex.foreach((param, index) =>
      functionEnvironment.define(param.lexeme, arguments(index))
    )

    try {
      functionDeclaration.body.foreach(stmt =>
        stmt.execute(functionEnvironment, locals)
      )
      // Override the return value inside init functions
      // No need to worry about return values, since that will be blocked during resolution phase
      if isInitializer then environment.getAt("this", 0)
    } catch {
      case e: ReturnException =>
        if isInitializer then environment.getAt("this", 0)
        else e.returnValue
    }

  }

  override def toString: String = s"fn: ${functionDeclaration.name.lexeme}"

  def bind(instance: LoxInstance): LoxCallable = {
    val boundEnvWithThis = Environment(environment)
    boundEnvWithThis.define("this", instance)
    FunctionCallable(
      boundEnvWithThis,
      locals,
      functionDeclaration,
      isInitializer
    )
  }
}
