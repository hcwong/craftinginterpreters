package runtime

import parser.{Environment, RuntimeError, Statement}
import LoxApp.LoxApp

class Interpreter(private val environment: Environment = Environment()) {
  def interpret(statement: Statement): Unit =
    try {
      given Environment = environment
      statement.execute()
    } catch {
      case e: RuntimeError => LoxApp.runtimeError(e)
      case _               => // let it pass
    }
}
