package runtime

import parser.Environment
import parser.Statement.FunctionDeclaration

case class FunctionCallable(
    private val functionDeclaration: FunctionDeclaration
) extends LoxCallable {
  override val arity: Int = functionDeclaration.parameters.size

  override def call(arguments: Seq[Any]): Any = {
    val environment = Environment(Environment.global)

    functionDeclaration.parameters.zipWithIndex.foreach((param, index) =>
      environment.define(param.lexeme, arguments(index))
    )

    functionDeclaration.body.foreach(stmt => stmt.execute(environment))
  }

  override def toString: String = s"fn: ${functionDeclaration.name.lexeme}"
}
