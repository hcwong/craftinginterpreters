package parser

import tokens.{Token, TokenType}

case class Parser(
    private var current: Int,
    private val tokens: List[Token]
) {
  private def equality: Expr = {
    var expr: Expr = comparison

    while(checkIfTokenTypesMatch(Seq(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))) {
      val operator = previous.tokenType
      val right = comparison
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison: Expr = {
    var expr: Expr = term

    while (checkIfTokenTypesMatch(Seq(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL))) {
      val operator = previous.tokenType
      val right = term
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def term: Expr = {
    var expr: Expr = factor

    while(checkIfTokenTypesMatch(Seq(TokenType.PLUS, TokenType.MINUS))) {
      val operator = previous.tokenType
      val right = factor
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def factor: Expr = {
    var expr: Expr = unary

    while(checkIfTokenTypesMatch(Seq(TokenType.SLASH, TokenType.STAR))) {
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
      val expr = unary()
      Expr.Unary(operator, expr)
    } else {
      primary()
    }
  }

  private def primary: Expr = {
    if (checkIfTokenTypeMatches(TokenType.FALSE)) {
      Expr.Literal(false)
    } else if (checkIfTokenTypeMatches(TokenType.TRUE)) {
      Expr.Literal(true)
    } else if (checkIfTokenTypeMatches(TokenType.NIL)) {
      Expr.Literal(null)
    } else if (checkIfTokenTypeMatches(TokenType.STRING)) {
      Expr.Literal(previous.literal.as[String])
    } else if (checkIfTokenTypeMatches(TokenType.NUMBER)) {
      Expr.Literal(previous.literal.as[Int])
    } else if (checkIfTokenTypeMatches(TokenType.LEFT_PAREN)) {
      // TODO: Handle Expr.Grouping
      sys.error("Unimplemented")
    }

    // TODO
    sys.error("Unrecognised token in Primary")
  }

  private def checkIfTokenTypesMatch(tokenTypes: Seq[TokenType]): Boolean = {
    for (tokenType <- tokenTypes) {
      if checkIfTokenTypeMatches(tokenType) then {
        advance()
        // Early return to avoid further iteration
        return true
      }
    }
    false
  }

  private def checkIfTokenTypeMatches(tokenType: TokenType): Boolean = {
    if (isAtEnd) false else peek.tokenType == tokenType
  }

  private def isAtEnd = peek.tokenType == TokenType.EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous;
  }
}
