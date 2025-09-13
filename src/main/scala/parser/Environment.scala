package parser

import runtime.LoxCallable
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

  // There should be no need to look up enclosing env as that's already performed during resolution
  def getAt(variableToken: Token, depth: Int): Any =
    ancestor(depth).variableMap
      .getOrElse(
        variableToken.lexeme,
        throw RuntimeError(
          variableToken,
          s"Missing variable after resolution: ${variableToken.lexeme}, when analyzing depth ${depth}"
        )
      )

  // Likewise, no need to lookup enclosing env
  def assignAt(variableName: String, value: Any, depth: Int): Any = {
    ancestor(depth).define(variableName, value)
    value
  }

  private def ancestor(depth: Int): Environment =
    (0 until depth).foldLeft(this)((env, _) => env.enclosing.get)

  def assign(variableToken: Token, value: Any): Any =
    if (variableMap.contains(variableToken.lexeme)) {
      define(variableToken.lexeme, value)
      value
    } else {
      enclosing match {
        case Some(enclosingEnv) =>
          enclosingEnv.assign(variableToken, value)
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
