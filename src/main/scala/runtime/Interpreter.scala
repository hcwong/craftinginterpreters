package runtime

import parser.{Environment, RuntimeError, Statement}
import LoxApp.LoxApp

class Interpreter(private val environment: Environment = Environment.global) {
  def interpret(statement: Statement): Unit =
    try {
      statement.execute(environment)
    } catch {
      case e: RuntimeError => LoxApp.runtimeError(e)
      case _               => // let it pass
    }
}
