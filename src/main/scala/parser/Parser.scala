package parser

import tokens.{Token, TokenType}

case class Parser(
    private var current: Int,
    private val tokens: List[Token]
) {
  private def equality: Expr = {
    var expr: Expr = comparison

    while (
      checkIfTokenTypesMatch(Seq(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))
    ) {
      val operator = previous.tokenType
      val right = comparison
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison: Expr = {
    var expr: Expr = term

    while (
      checkIfTokenTypesMatch(
        Seq(
          TokenType.GREATER,
          TokenType.GREATER_EQUAL,
          TokenType.LESS,
          TokenType.LESS_EQUAL
        )
      )
    ) {
      val operator = previous.tokenType
      val right = term
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def term: Expr = {
    var expr: Expr = factor

    while (checkIfTokenTypesMatch(Seq(TokenType.PLUS, TokenType.MINUS))) {
      val operator = previous.tokenType
      val right = factor
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def factor: Expr = {
    var expr: Expr = unary

    while (checkIfTokenTypesMatch(Seq(TokenType.SLASH, TokenType.STAR))) {
      val operator = previous.tokenType
      val right = unary
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def unary: Expr = {
    if (checkIfTokenTypesMatch(Seq(TokenType.BANG, TokenType.MINUS))) {
      val operator = previous.tokenType
      // Nothing stopping a user from doing !!true, so call unary again
      val expr = unary
      Expr.Unary(operator, expr)
    } else {
      primary
    }
  }

  private def primary: Expr = {
    if (!isAtEnd) {
      peek.tokenType match {
        case TokenType.FALSE => Expr.Literal(false)
        case TokenType.TRUE  => Expr.Literal(true)
        case TokenType.NIL   => Expr.Literal(null)
        case TokenType.STRING =>
          previous.literal match {
            case s: String => Expr.Literal(s)
            case _ =>
              sys.error(
                s"Unrecognised literal ${previous.literal} when expecting String"
              )
          }
        case TokenType.NUMBER =>
          previous.literal match {
            case i: Int => Expr.Literal(i)
            case _ =>
              sys.error(
                s"Unrecognised literal ${previous.literal} when expecting Int"
              )
          }
        case TokenType.LEFT_PAREN =>
          sys.error("Unimplemented behaviour for Expr.Grouping")
        case _ =>
          sys.error(s"Unrecognised token type ${peek.tokenType} in primary")
      }
    } else {
      // TODO: handle errors gracefully
      sys.error("Encountered end of tokens but expecting literal Expr")
    }
  }

  private def checkIfTokenTypesMatch(tokenTypes: Seq[TokenType]): Boolean =
    tokenTypes.find(checkIfTokenTypeMatches) match {
      case Some(_) =>
        advance()
        true
      case _ => false
    }

  private def checkIfTokenTypeMatches(tokenType: TokenType): Boolean = {
    if (isAtEnd) false else peek.tokenType == tokenType
  }

  private def isAtEnd = peek.tokenType == TokenType.EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }
}
