package LoxApp

import scala.io.StdIn.readLine
import scanner.Scanner
import tokens.{Token, TokenType}
import parser.{Parser, RuntimeError}
import runtime.Interpreter

object LoxApp {
  private var hadError: Boolean = false
  private var hadRuntimeError: Boolean = false

  def main(args: Array[String]): Unit = {
    val argLen = args.length
    if (argLen > 1) {
      println("Usage: slox [script]")
      sys.exit(64)
    } else if (argLen == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  def runFile(path: String): Unit = {
    run(os.read(os.Path(path, os.pwd)))

    if (hadError) {
      sys.exit(65)
    } else if (hadRuntimeError) {
      sys.exit(70)
    }
  }

  def runPrompt(): Unit = {
    while (true) {
      print("> ")
      val line = readLine()
      line match {
        case null => return
        case ""   => ()
        case _    => run(line)
      }
      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val tokens = Scanner(source).scanTokens().toSeq
    val parser = Parser(tokens = tokens)
    val exprOpt = parser.parse

    if (hadError) {
      ()
    } else {
      exprOpt.foreach(expr => Interpreter.interpret(expr))
      ()
    }
  }

  def error(line: Long, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit = {
    if (token.tokenType == TokenType.EOF) {
      report(token.line, "at end", message)
    } else {
      report(token.line, s"at '${token.lexeme}'", message)
    }
  }

  def runtimeError(error: RuntimeError): Unit = {
    System.err.println(s"${error.message}\n[line: ${error.token.line}]")
    hadRuntimeError = true
  }

  def report(line: Long, where: String, message: String): Unit = {
    System.err.println(s"[line: ${line}] Error ${where}: ${message}")
    hadError = true
  }
}
