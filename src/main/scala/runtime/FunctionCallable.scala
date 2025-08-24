package runtime

import parser.Environment
import parser.Statement.FunctionDeclaration

case class FunctionCallable(
    environment: Environment,
    private val functionDeclaration: FunctionDeclaration
) extends LoxCallable {
  override val arity: Int = functionDeclaration.parameters.size

  override def call(arguments: Seq[Any]): Any = {
    functionDeclaration.parameters.zipWithIndex.foreach((param, index) =>
      environment.define(param.lexeme, arguments(index))
    )

    try {
      functionDeclaration.body.foreach(stmt => stmt.execute(environment))
    } catch {
      case e: ReturnException => e.returnValue
    }
  }

  override def toString: String = s"fn: ${functionDeclaration.name.lexeme}"
}
