package runtime

import parser.RuntimeError
import tokens.Token

import scala.collection.mutable

case class LoxKlass(name: String) extends LoxCallable {
  override def toString: String = name

  val arity: Int = 0

  override def call(arguments: Seq[Any]): Any = LoxInstance(klass = this)
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
  private val properties: mutable.Map[String, Object] = mutable.Map.empty

  // Lox getters, like most getters, should chain
  def get(propertyToken: Token): Object = {
    properties.get(propertyToken.lexeme) match {
      case Some(value) => value
      case None =>
        throw RuntimeError(
          propertyToken,
          s"Tried to get property ${propertyToken.lexeme} but it does not exist"
        )
    }
  }

  override def toString: String = s"${klass.name} instance"
}
