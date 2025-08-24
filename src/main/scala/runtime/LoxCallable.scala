package runtime

import parser.RuntimeError
import tokens.Token

trait LoxCallable {
  protected val arity: Int

  def call(arguments: Seq[Any]): Any

  private def validateArityAndCall(
      arguments: Seq[Any],
      closingParen: Token
  ): Any = {
    if (arity != arguments.size) {
      throw RuntimeError(
        closingParen,
        s"Expected $arity parameters but received ${arguments.size} arguments"
      )
    } else {
      call(arguments)
    }
  }
}
