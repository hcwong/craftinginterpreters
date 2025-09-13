package runtime

import parser.RuntimeError
import tokens.Token

import scala.collection.mutable

case class LoxKlass(name: String, methods: Map[String, FunctionCallable])
    extends LoxCallable {
  override def toString: String = name

  val arity: Int = 0

  override def call(arguments: Seq[Any]): Any = LoxInstance(klass = this)

  def findMethod(name: String): Option[FunctionCallable] = methods.get(name)
}

case class LoxInstance(klass: LoxKlass) {
  // In "looser" OOP languages like Lox, we use a hash table lookup to get the properties
  // This is fine for most cases
  // However, this degenerates into multiple instruction calls and pointer jumps to resolve the value of the property
  // For high performance scenarios, we want C-like structs behaviour where properties are stored in fixed order,
  // similar to what Java's Project Valhalla.
  // This ensures that data is colocated in the same cache line and doesn't need pointer jumping
  // thus enabling it to be resolved in 1 instruction (probably by calculating offsets)
  // https://richardartoul.github.io/jekyll/update/2015/04/26/hidden-classes.html for more information
  private val fields: mutable.Map[String, Any] = mutable.Map.empty

  // Lox getters, like most getters, should chain
  def get(propertyToken: Token): Any = {
    (
      fields.get(propertyToken.lexeme),
      klass.findMethod(propertyToken.lexeme)
    ) match {
      // Preferentially resolve to field first, thus supporting "shadow methods"
      case (Some(fieldValue), Some(_)) =>
        fieldValue
      case (None, Some(methodValue)) => methodValue
      case (Some(fieldValue), None)  => fieldValue
      case (None, None) =>
        throw RuntimeError(
          propertyToken,
          s"Tried to get property ${propertyToken.lexeme} but it does not exist"
        )
    }
  }

  def set(propertyToken: Token, value: Any): Any = {
    fields.addOne(
      propertyToken.lexeme,
      value
    )
    value
  }

  override def toString: String = s"${klass.name} instance"
}
