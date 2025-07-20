package scanner

import tokens.{Token, TokenType}
import LoxApp.LoxApp

import scala.collection.mutable.ArrayBuffer

/** This class translates a source (file or CLI Repl) into Tokens which can
  * later be operated on
  */
class Scanner(source: String) {
  private[this] val tokens: ArrayBuffer[Token] = ArrayBuffer.empty
  private[this] var start: Int = 0
  private[this] var current: Int = 0
  private[this] var line: Int = 0

  final private[this] val keywords: Map[String, TokenType] = Map(
    "and" -> TokenType.AND,
    "class" -> TokenType.CLASS,
    "else" -> TokenType.ELSE,
    "false" -> TokenType.FALSE,
    "for" -> TokenType.FOR,
    "fun" -> TokenType.FUN,
    "if" -> TokenType.IF,
    "nil" -> TokenType.NIL,
    "or" -> TokenType.OR,
    "print" -> TokenType.PRINT,
    "return" -> TokenType.RETURN,
    "super" -> TokenType.SUPER,
    "this" -> TokenType.THIS,
    "true" -> TokenType.TRUE,
    "var" -> TokenType.VAR,
    "while" -> TokenType.WHILE
  )

  def scanTokens(): ArrayBuffer[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken()
    }

    this.tokens += Token(TokenType.EOF, "", null, line)
    this.tokens
  }

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
      case '?'                     => addToken(TokenType.QUESTION_MARK)
      case ':'                     => addToken(TokenType.COLON)
      case '!' if matchesChar('=') => addToken(TokenType.BANG_EQUAL)
      case '!'                     => addToken(TokenType.BANG)
      case '=' if matchesChar('=') => addToken(TokenType.EQUAL_EQUAL)
      case '='                     => addToken(TokenType.EQUAL)
      case '<' if matchesChar('=') => addToken(TokenType.LESS_EQUAL)
      case '<'                     => addToken(TokenType.LESS)
      case '>' if matchesChar('=') => addToken(TokenType.GREATER_EQUAL)
      case '>'                     => addToken(TokenType.GREATER)
      case '/' if matchesChar('/') =>
        while (peek() != '\n' && !isAtEnd) {
          advance()
        }
      case '/' if matchesChar('*') => multiLineComment()
      case '/' =>
        addToken(TokenType.SLASH)
      case '"' => string()
      // whitespace
      case ' '  => ()
      case '\r' => ()
      case '\t' => ()
      case '\n' =>
        line += 1
        ()
      case c if c.isDigit              => number()
      case c if c.isLetter || c == '_' => identifier()
      case char => LoxApp.error(line, f"Unexpected character: $char")
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

  private def addToken(tokenType: TokenType, literal: Any): Unit = {
    val text = source.substring(start, current)
    this.tokens += Token(tokenType, text, literal, line)
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

  private def peek(): Char = {
    if (isAtEnd) '\u0000' else source.charAt(current)
  }

  private def peekNext(): Char = {
    // If exceeds length of source, return null character
    if (current + 1 >= source.length) '\u0000'
    else source.charAt(current + 1)
  }

  private def string(): Unit = {
    while (peek() != '"' && !isAtEnd) {
      if (peek() == '\n') line += 1

      advance()
    }

    if (isAtEnd) {
      LoxApp.error(line, "Unterminated string")
    }

    advance()

    val s = source.substring(start + 1, current - 1)
    addToken(TokenType.STRING, s)
  }

  private def number(): Unit = {
    while (peek().isDigit) advance()

    // Looks for decimal point
    if (peek() == '.' && peekNext().isDigit) advance()

    while (peek().isDigit) advance()

    addToken(
      TokenType.NUMBER,
      source.substring(start, current).toDouble
    )
  }

  private def identifier(): Unit = {
    while (peek().isLetterOrDigit) advance()

    val maybeKeyword = keywords.get(source.substring(start, current))
    maybeKeyword match {
      case Some(tokenType) => addToken(tokenType)
      // User defined identifier
      case None =>
        addToken(TokenType.IDENTIFIER, source.substring(start, current))
    }
  }

  def multiLineComment(): Unit = {
    while (peek() != '*' && peekNext() != '/' && !isAtEnd) {
      if (peek() == '\n') line += 1

      advance()
    }

    if (isAtEnd) {
      ()
    } else {
      // Advance twice to consume */
      advance()
      advance()
    }
  }
}
