package runtime

import parser.{Statement, RuntimeError}
import LoxApp.LoxApp

object Interpreter {
  def interpret(statement: Statement): Unit =
    try {
      statement.execute()
    } catch {
      case e: RuntimeError => LoxApp.runtimeError(e)
      case _               => // let it pass
    }
}
