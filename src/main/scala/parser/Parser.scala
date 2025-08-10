package parser

import tokens.{Token, TokenType}
import LoxApp.LoxApp
import parser.Expr.{Assignment, Variable}
import parser.Statement.WhileStatement

import scala.annotation.tailrec
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
    val statements = mutable.Buffer[Option[Statement]]()
    while (!isAtEnd) {
      statements.append(declaration)
    }
    statements.flatten.toSeq
  }

  // The complete grammar is as follows expression -> ternary
  // program -> declaration * EOF
  // declaration -> function declaration | variabledeclaration | statement | block statement
  // function declaration -> "fun" function
  // function -> IDENTIFIER '(' parameters? ')' block
  // parameters -> IDENTIFIER ( ',' IDENTIFIER )*
  // variabledeclaration -> 'var' IDENTIFIER (= expr)? ;
  // statement -> exprstatement | ifstatement |  printstatement | if stmt | while stmt | for statement
  // exprstatement -> expr ;
  // printstatement -> print expr ;
  // “ifStmt         → "if" "(" expression ")" statement ( "else" statement )?
  // while stmt -> while (expression) statement;
  // for stmt -< “for" "(" ( varDecl | exprStmt | ";" )?
  //                 expression? ";"
  //                 expression? ")" statement ”
  // block statement -> '{' (declaration)* '}'
  // expr -> assignment
  // assignment -> IDENTIFIER '=' assignment | logic_or   (doesn't this grammar allow for multiple assignment statements to be chained? Not wrong I guess)
  // logic_or -> logic_and (OR logic_and )*
  // logic_and -> ternary (AND ternary)*
  // ternary -> equality ('?' expr ':' expr)*
  // equality -> comparison ( '!=' | '==' comparison )*
  // comparison -> term ('>=' | '<=' | '>' | '<' term)*
  // term -> factor ( '+' | '-' factor)* factors are higher precedence than term due to BODMAS
  // factor -> unary ('*' | '/' unary)*
  // unary -> ('-' | '!' unary)* | call
  // call -> primary ( '(' arguments? ')' )*
  // arguments -> expression ( ',' expression )*
  // primary -> boolean | number | string | null | parentheses expression (expr) | IDENTIFIER
  //
  // Note that we are careful to avoid left recursion, and in no part of the
  // grammar do we call the same rule as the first term. Else we would stack
  // overflow.

  private def declaration: Option[Statement] = {
    try {
      if (checkAndAdvance(Seq(TokenType.VAR))) {
        Some(variableDeclaration())
      } else {
        Some(statement)
      }
    } catch {
      case _: ParseException =>
        synchronize()
        None
    }
  }

  // Kind: Could be method, function etc
  private def functionDeclaration(kind: String): Statement = {
    val identifier = consume(
      TokenType.IDENTIFIER,
      s"Expected $kind name"
    )

    consume(TokenType.LEFT_PAREN, s"Expected ( after $kind name")
    val parameters = getFunctionParams(kind, mutable.ArrayBuffer.empty).toSeq

    consume(
      TokenType.LEFT_BRACE,
      s"Expected { after defining parameters for $kind"
    )
    val functionBody = getStatementsInBlock()

    Statement.FunctionDeclaration(
      identifier,
      parameters,
      getStatementsInBlock()
    )
  }

  @tailrec
  private def getFunctionParams(
      kind: String,
      parameters: mutable.ArrayBuffer[Token]
  ): mutable.ArrayBuffer[Token] = {
    if parameters.size > Parser.MAX_ARG_SIZE then
      throw error(
        peek,
        s"$kind can't have more than ${Parser.MAX_ARG_SIZE} paramters"
      )
    else if checkAndAdvance(TokenType.RIGHT_PAREN) then parameters
    else {
      consume(TokenType.COMMA, s"Expected , between $kind parameters")
      parameters.append(
        consume(TokenType.IDENTIFIER, s"Expected $kind parameter after comma")
      )
      getFunctionParams(kind, parameters)
    }
  }

  private def variableDeclaration(): Statement = {
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
    Statement.VariableDeclaration(identifier, expr)
  }

  private def statement: Statement = {
    if (checkAndAdvance(Seq(TokenType.PRINT))) {
      val expr = expression
      consume(TokenType.SEMICOLON, "Expected ; after print statement")
      Statement.PrintStatement(expr)
    } else if (checkAndAdvance(Seq(TokenType.IF))) {
      consume(TokenType.LEFT_PAREN, "Expected ( after if keyword")
      val conditional = expression
      consume(TokenType.RIGHT_PAREN, "Expected ) after if condition")
      val ifClause = expression
      val elseClause = if (checkAndAdvance(Seq(TokenType.ELSE))) {
        Some(expression)
      } else None
      Statement.IfStatement(conditional, ifClause, elseClause)
    } else if (checkAndAdvance(Seq(TokenType.LEFT_BRACE))) {
      Statement.BlockStatement(getStatementsInBlock())
    } else if (checkAndAdvance(TokenType.WHILE)) {
      whileStatement()
    } else if (checkAndAdvance(TokenType.FOR)) {
      forStatement()
    } else {
      expressionStatement()
    }
  }

  private def getStatementsInBlock(): Seq[Statement] = {
    val blockStatements = mutable.ArrayBuffer[Option[Statement]]()

    while (!isAtEnd && !checkType(TokenType.RIGHT_BRACE)) {
      blockStatements.append(declaration)
    }

    consume(TokenType.RIGHT_BRACE, "Expected } after block")
    blockStatements.flatten.toSeq
  }

  private def expressionStatement(): Statement = {
    val expr = expression
    consume(TokenType.SEMICOLON, "Expected ; after expression statement")
    Statement.ExprStatement(expr)
  }

  private def whileStatement(): Statement = {
    consume(TokenType.LEFT_PAREN, "Expected ( after while keyword")
    val condition = expression
    consume(TokenType.RIGHT_PAREN, "Expected ) after while expression")
    Statement.WhileStatement(condition, statement)
  }

  private def forStatement(): Statement = {
    // In Lox syntax, any of the initializer, terminating condition, or increment can be omitted
    consume(TokenType.LEFT_PAREN, "Expected ( after for keyword")

    val initializer = if (checkAndAdvance(TokenType.SEMICOLON)) {
      None
    } else if (checkAndAdvance(TokenType.VAR)) {
      Some(variableDeclaration())
    } else {
      Some(expressionStatement())
    }

    val terminatingConditionOpt = if (checkAndAdvance(TokenType.SEMICOLON)) {
      None
    } else {
      val terminatingExpr = Some(expression)
      consume(
        TokenType.SEMICOLON,
        "Expected ; after for loop terminating condition"
      )
      terminatingExpr
    }
    val terminatingCondition =
      terminatingConditionOpt.getOrElse(Expr.Literal(true))

    val increment = if (checkAndAdvance(TokenType.RIGHT_PAREN)) {
      None
    } else {
      val expr = expression
      consume(TokenType.RIGHT_PAREN, "Expected ) after for loop conditions")
      Some(Statement.ExprStatement(expr))
    }

    val statementBody = statement

    // If initializer is present, run it once before the while loop
    // Terminating condition is already vacuously set
    // If increment is None, just omit it and do not run it after the statementBody
    val whileLoopBody =
      Statement.BlockStatement(Seq(Some(statementBody), increment).flatten)
    val whileLoop =
      Statement.WhileStatement(terminatingCondition, whileLoopBody)
    Statement.BlockStatement(
      Seq(initializer, Some(whileLoop)).flatten
    )
  }

  private def expression: Expr = assignment

  private def assignment: Expr = {
    // Cannot be assignment here else we will recurse all the way down
    // perks of left recursive descent parser
    val expr = logicOr

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

  def logicOr: Expr = {
    val expr = logicAnd
    if (checkAndAdvance(Seq(TokenType.OR))) {
      val subsequentClause = logicOr
      Expr.Logical(expr, Expr.LogicalOperator.Or, subsequentClause)
    } else {
      expr
    }
  }

  def logicAnd: Expr = {
    val expr = ternaryOperator
    if (checkAndAdvance(Seq(TokenType.AND))) {
      val subsequentClause = logicAnd
      Expr.Logical(expr, Expr.LogicalOperator.And, subsequentClause)
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
      call()
    }
  }

  private def call(): Expr = {
    val expr = primary

    if (checkAndAdvance(TokenType.LEFT_PAREN)) {
      callRec(primary, Some(mutable.ArrayBuffer.empty[Expr]))
    } else {
      expr
    }
  }

  // Not the easiest to read, but challenging myself to avoid while true + break patterns
  @tailrec
  private def callRec(
      expr: Expr,
      arguments: Option[mutable.ArrayBuffer[Expr]]
  ): Expr = {
    arguments match {
      case Some(args) if args.size > Parser.MAX_ARG_SIZE =>
        throw error(
          peek,
          s"Can't have more than ${Parser.MAX_ARG_SIZE} arguments, friend"
        )
      // If arguments is a Non-none value, we are actively looking for the closing brace
      // So we want to search for either closing brace OR expr
      case Some(args) =>
        if checkAndAdvance(TokenType.RIGHT_PAREN) then
          callRec(Expr.Call(expr, previous, args.toSeq), None)
        else {
          // If its not a closing brace, expect a comma before the next expression
          consume(TokenType.COMMA, "Expected , between arguments")
          args.append(expression)
          callRec(expr, Some(args))
        }
      // Now, if arguments is None, there are two cases
      // If the next token is left paren restart the Some(args) clause again
      // Else we hit base case and can return expr
      case None =>
        if checkAndAdvance(TokenType.LEFT_PAREN) then
          callRec(expr, Some(mutable.ArrayBuffer.empty[Expr]))
        else expr
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
          throw error(peek, s"Expected expression")
      }

  private def checkAndAdvance(tokenType: TokenType): Boolean = {
    if (checkType(tokenType)) {
      advance()
      true
    } else {
      false
    }
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

object Parser {
  // Well, follow Scala 2 for simplicity. No particular reason why, but why do you need
  // a 23 arity function?
  private val MAX_ARG_SIZE = 22
}

private[parser] case class ParseException() extends RuntimeException
