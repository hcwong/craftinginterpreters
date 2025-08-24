package parser

import runtime.{Interpreter, LoxCallable}
import tokens.Token

class Environment(
    private val variableMap: scala.collection.mutable.Map[String, Any] =
      scala.collection.mutable.Map.empty,
    private val enclosing: Option[Environment] = None
) {

  def define(variableName: String, value: Any): Unit =
    variableMap.addOne((variableName, value))

  def get(variableToken: Token): Any =
    (variableMap.get(variableToken.lexeme), enclosing) match {
      case (Some(value), _)           => value
      case (None, Some(enclosingEnv)) => enclosingEnv.get(variableToken)
      case _ =>
        throw RuntimeError(
          variableToken,
          s"Undefined variable ${variableToken.lexeme}."
        )
    }

  def assign(variableToken: Token, value: Any): Any =
    if (variableMap.contains(variableToken.lexeme)) {
      define(variableToken.lexeme, value)
      value
    } else {
      enclosing match {
        case Some(enclosingEnv) => enclosingEnv.get(variableToken)
        case None =>
          throw RuntimeError(
            variableToken,
            s"Unable to assign to undeclared variable ${variableToken.lexeme}"
          )
      }
    }
}

object Environment {
  final val global: Environment = {
    val _global = Environment()
    _global.define(
      "clock",
      new LoxCallable {
        override protected val arity: Int = 0

        override def call(arguments: Seq[Any]): Any =
          System.currentTimeMillis().toDouble / 1000.0
      }
    )
    _global
  }

  private def apply(): Environment = new Environment()

  def apply(enclosing: Environment) =
    new Environment(enclosing = Some(enclosing))
}
