package parser

sealed trait Statement

object Statement {
  // Print statements aren't usually part of the compiler, implemented by libs
  case class PrintStatement(expr: Expr) extends Statement
  case class ExprStatement(expr: Expr) extends Statement

  extension (statement: Statement) {
    def execute(): Unit = {
      statement match {
        case PrintStatement(expr) =>
          println(expr.evaluate)
        case ExprStatement(expr) => expr.evaluate
      }
      ()
    }
  }
}
