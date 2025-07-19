package runtime

import parser.{Expr, RuntimeError}
import LoxApp.LoxApp

object Interpreter {
  def interpret(expr: Expr): Unit =
    try {
      println(expr.evaluate)
    } catch {
      case e: RuntimeError => LoxApp.runtimeError(e)
      case _               => // let it pass
    }
}
