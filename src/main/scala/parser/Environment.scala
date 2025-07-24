package parser

import tokens.Token

class Environment(
    private val variableMap: scala.collection.mutable.Map[String, Any] =
      scala.collection.mutable.Map.empty
) {
  def define(variableName: String, value: Any): Unit =
    variableMap.addOne((variableName, value))

  def get(variableToken: Token): Any =
    variableMap.get(variableToken.lexeme) match {
      case Some(value) => value
      case None =>
        throw RuntimeError(
          variableToken,
          s"Undefined variable ${variableToken.lexeme}."
        )
    }
}
