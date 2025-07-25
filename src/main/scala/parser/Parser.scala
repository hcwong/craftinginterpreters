package parser

import tokens.{Token, TokenType}
import LoxApp.LoxApp
import parser.Expr.{Assignment, Variable}

import scala.collection.mutable

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

  def parse: Seq[Statement] = {
    val statements = mutable.Buffer[Statement]()
    var hasParseError: Boolean = false
    while (!isAtEnd) {
      try {
        statements.append(declaration)
      } catch { case _: ParseException => hasParseError = true }
    }
    if !hasParseError then statements.toSeq else Seq.empty
  }

  // The complete grammar is as follows expression -> ternary
  // program -> declaration * EOF
  // declaration -> variabledeclaration | statement
  // variabledeclaration -> 'var' IDENTIFIER (= expr)? ;
  // statement -> exprstatement | printstatement
  // exprstatement -> expr ;
  // printstatement -> print expr ;
  // expr -> assignment
  // assignment -> IDENTIFIER '=' assignment | ternary   (doesn't this grammar allow for multiple assignment statements to be chained? Not wrong I guess)
  // ternary -> equality ('?' expr ':' expr)*
  // equality -> comparison ( '!=' | '==' comparison )*
  // comparison -> term ('>=' | '<=' | '>' | '<' term)*
  // term -> factor ( '+' | '-' factor)* factors are higher precedence than term due to BODMAS
  // factor -> unary ('*' | '/' unary)*
  // unary -> ('-' | '!' unary)* | primary
  // primary -> boolean | number | string | null | parentheses expression (expr) | IDENTIFIER
  //
  // Note that we are careful to avoid left recursion, and in no part of the
  // grammar do we call the same rule as the first term. Else we would stack
  // overflow.

  private def declaration: Statement = {
    if (checkAndAdvance(Seq(TokenType.VAR))) {
      val identifier = consume(
        TokenType.IDENTIFIER,
        "Expected variable name after var keyword"
      )
      val expr = if (checkAndAdvance(Seq(TokenType.EQUAL))) {
        Some(expression)
      } else {
        None
      }
      consume(TokenType.SEMICOLON, "Expected ; after variable declaration")
      VariableDeclaration(identifier, expr)
    } else {
      statement
    }
  }

  private def statement: Statement = {
    if (checkAndAdvance(Seq(TokenType.PRINT))) {
      val expr = expression
      consume(TokenType.SEMICOLON, "Expected ; after print statement")
      Statement.PrintStatement(expr)
    } else {
      val expr = expression
      consume(TokenType.SEMICOLON, "Expected ; after expression statement")
      Statement.ExprStatement(expr)
    }
  }

  private def expression: Expr = assignment

  private def assignment: Expr = {
    // Cannot be assignment here else we will recurse all the way down
    // perks of left recursive descent parser
    val expr = ternaryOperator

    if (checkAndAdvance(Seq(TokenType.EQUAL))) {
      val equalityToken = previous
      val assignmentExpr = assignment

      expr match {
        case Variable(identifierToken) =>
          Assignment(identifierToken, assignmentExpr)
        case _ =>
          error(equalityToken, s"Invalid assignment target: ${expr.print}")
          expr
      }
    } else {
      expr
    }
  }

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

        case TokenType.IDENTIFIER =>
          advance()
          previous.literal match
            case str: String => Expr.Variable(previous)
            case _ =>
              throw error(
                previous,
                s"Expected string as variable name but encountered ${previous.literal}"
              )

        case _ =>
          advance()
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
