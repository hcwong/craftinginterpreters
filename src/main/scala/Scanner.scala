package scanner

import tokens.{Token, TokenType}
import LoxApp.LoxApp

class Scanner(source: String) {
  private[this] val tokens: Seq[Token] = Seq.empty
  private[this] val start: Int = 0
  private[this] var current: Int = 0
  private[this] var line: Int = 0

  private def scanToken(): Unit = {
    val c = advance()
    c match {
      case '('                     => addToken(TokenType.LEFT_PAREN)
      case ')'                     => addToken(TokenType.RIGHT_PAREN)
      case '{'                     => addToken(TokenType.LEFT_BRACE)
      case '}'                     => addToken(TokenType.RIGHT_BRACE)
      case ','                     => addToken(TokenType.COMMA)
      case '.'                     => addToken(TokenType.DOT)
      case '-'                     => addToken(TokenType.MINUS)
      case '+'                     => addToken(TokenType.PLUS)
      case ';'                     => addToken(TokenType.SEMICOLON)
      case '*'                     => addToken(TokenType.STAR)
      case '!' if matchesChar('=') => addToken(TokenType.BANG_EQUAL)
      case '!'                     => addToken(TokenType.BANG)
      case '=' if matchesChar('=') => addToken(TokenType.EQUAL_EQUAL)
      case '='                     => addToken(TokenType.EQUAL)
      case '<' if matchesChar('=') => addToken(TokenType.LESS_EQUAL)
      case '<'                     => addToken(TokenType.LESS)
      case '>' if matchesChar('=') => addToken(TokenType.GREATER_EQUAL)
      case '>'                     => addToken(TokenType.GREATER)
      case char                    => LoxApp.error(line, f"Unexpected character: $char")
    }
  }

  private def isAtEnd: Boolean = current >= source.length

  // Consume and advance pointer
  private def advance(): Char = {
    val char = source.charAt(current)
    current += 1
    char
  }

  private def addToken(tokenType: TokenType): Unit = addToken(tokenType, null)

  private def addToken(tokenType: TokenType, literal: AnyRef): Unit = {
    val text = source.substring(start, current)
    tokens :+ (Token(tokenType, text, literal, line))
    ()
  }

  // Conditional advance
  private def matchesChar(expected: Char): Boolean = {
    if (isAtEnd) false
    else if (source.charAt(current) != expected) false
    else {
      current += 1
      true
    }
  }
}
