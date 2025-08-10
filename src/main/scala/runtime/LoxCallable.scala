package runtime

import parser.RuntimeError
import tokens.Token

trait LoxCallable {
  protected val arity: Int
  protected val closingParen: Token

  def call(interpreter: Interpreter, arguments: Seq[Any]): Any

  private def validateArityAndCall(
      interpreter: Interpreter,
      arguments: Seq[Any]
  ): Any = {
    if (arity != arguments.size) {
      throw RuntimeError(
        closingParen,
        s"Expected $arity parameters but received ${arguments.size} arguments"
      )
    } else {
      call(interpreter, arguments)
    }
  }
}
