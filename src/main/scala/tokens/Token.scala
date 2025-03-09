package tokens

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: AnyRef,
    line: Long
) {
  override def toString: String = s"$tokenType $lexeme $literal"
}
