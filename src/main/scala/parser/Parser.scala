package parser

import tokens.{Token, TokenType}
import LoxApp.LoxApp

/** This app contains the recursive descent logic that translates the list of
  * tokens into expressions to operate on Each rule only matches expressions at
  * its precedence level or higher. Remember that expressions with higher
  * precedence are evaluated first Recursive descent parsers walk down the tree,
  * but we always start with the lowest precedence exprs first because they may
  * contain exprs of higher precedence within them.
  * @param current
  *   current index of tokens
  * @param tokens
  *   list of tokens
  */
class Parser(
    private val tokens: Seq[Token],
    private var current: Int = 0
) {

  def parse: Option[Expr] = {
    try Some(expression)
    catch case _: ParseException => None
  }

  // The complete grammar is as follows expression -> ternary
  // ternary -> equality ('?' expr ':' expr)*
  // equality -> comparison ( '!=' | '==' comparison )*
  // comparison -> term ('>=' | '<=' | '>' | '<' term)*
  // term -> factor ( '+' | '-' factor)* factors are higher precedence than term due to BODMAS
  // factor -> unary ('*' | '/' unary)*
  // unary -> ('-' | '!' unary)* | primary
  // primary -> boolean | number | string | null | parentheses expression (expr)
  //
  // Note that we are careful to avoid left recursion, and in no part of the
  // grammar do we call the same rule as the first term. Else we would stack
  // overflow.

  private def expression: Expr = ternaryOperator

  private def ternaryOperator: Expr = {
    var expr: Expr = equality

    while (checkAndAdvance(Seq(TokenType.QUESTION_MARK))) {
      val positive = equality
      consume(TokenType.COLON, "expected ':' for a ternary operator")
      val negative = equality
      expr = Expr.Ternary(expr, positive, negative)
    }

    expr
  }

  private def equality: Expr = {
    var expr: Expr = comparison

    while (checkAndAdvance(Seq(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))) {
      val operator = previous
      val right = comparison
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison: Expr = {
    var expr: Expr = term

    while (
      checkAndAdvance(
        Seq(
          TokenType.GREATER,
          TokenType.GREATER_EQUAL,
          TokenType.LESS,
          TokenType.LESS_EQUAL
        )
      )
    ) {
      val operator = previous
      val right = term
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def term: Expr = {
    var expr: Expr = factor

    while (checkAndAdvance(Seq(TokenType.PLUS, TokenType.MINUS))) {
      val operator = previous
      val right = factor
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def factor: Expr = {
    var expr: Expr = unary

    while (checkAndAdvance(Seq(TokenType.SLASH, TokenType.STAR))) {
      val operator = previous
      val right = unary
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def unary: Expr = {
    if (checkAndAdvance(Seq(TokenType.BANG, TokenType.MINUS))) {
      val operator = previous
      // Nothing stopping a user from doing !!true, so call unary again
      val expr = unary
      Expr.Unary(operator, expr)
    } else {
      primary
    }
  }

  private def primary: Expr =
    if isAtEnd then throw error(peek, "Encountered EOF during parsing")
    else
      peek.tokenType match {
        case TokenType.FALSE =>
          advance()
          Expr.Literal(false)

        case TokenType.TRUE =>
          advance()
          Expr.Literal(true)

        case TokenType.NIL =>
          advance()
          Expr.Literal(null)

        case TokenType.STRING =>
          advance()
          previous.literal match
            case s: String => Expr.Literal(s)
            case other =>
              throw error(previous, s"Expected String but got $other")

        case TokenType.NUMBER =>
          advance()
          previous.literal match
            case i: Double => Expr.Literal(i)
            case other =>
              throw error(previous, s"Expected Number but got $other")

        case TokenType.LEFT_PAREN =>
          advance()
          val expr = expression
          consume(TokenType.RIGHT_PAREN, "Expected ')' after expression")
          Expr.Grouping(expr)

        case _ =>
          throw error(peek, "Expected expression")
      }

  private def checkAndAdvance(tokenTypes: Seq[TokenType]): Boolean =
    tokenTypes.find(checkType) match {
      case Some(_) =>
        advance()
        true
      case _ => false
    }

  private def checkType(tokenType: TokenType): Boolean = {
    if (isAtEnd) false else peek.tokenType == tokenType
  }

  private def isAtEnd = peek.tokenType == TokenType.EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }

  private def consume(tokenType: TokenType, message: String): Token = {
    if (checkType(tokenType)) {
      advance()
    } else {
      throw error(peek, message)
    }
  }

  private def error(token: Token, message: String): ParseException = {
    LoxApp.error(token, message)
    ParseException()
  }

  /** Call this method to attempt to synchronize the parser after encountering a
    * parser error Basically discards all tokens until we find a new statement
    * boundary Anything that is discarded would probably just have been a
    * cascaded parser error anyways
    */
  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      // TODO: Rewrite this less imperatively so I don't use return, maybe use TCO
      if (previous.tokenType == TokenType.SEMICOLON) {
        return
      } else {
        if (
          Set(
            TokenType.CLASS,
            TokenType.FUN,
            TokenType.VAR,
            TokenType.FOR,
            TokenType.IF,
            TokenType.WHILE,
            TokenType.RETURN
          ).contains(peek)
        ) {
          return
        }
      }
    }

    ()
  }
}

private[parser] case class ParseException() extends RuntimeException
