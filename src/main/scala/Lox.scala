package LoxApp

import scanner.Scanner
import tokens.{Token, TokenType}
import parser.{Parser, RuntimeError}
import runtime.Interpreter

import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

object LoxApp {
  private var hadError: Boolean = false
  private var hadRuntimeError: Boolean = false
  // Move Interpreter to top level scope so that it keeps state for REPL mode
  private val interpreter = Interpreter()

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
    val terminal = TerminalBuilder
      .builder()
      .system(true) // Use system terminal
      .build()

    val reader: LineReader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .build()

    while (true) {
      val line = reader.readLine("> ")
      line match {
        case null   => return
        case "exit" => return
        case ""     => ()
        case _      => run(line)
      }
      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val tokens = Scanner(source).scanTokens().toSeq
    val parser = Parser(tokens = tokens)
    val statements = parser.parse

    if (hadError) {
      ()
    } else {
      statements.foreach(stmt => interpreter.resolve(stmt))
      statements.foreach(stmt => interpreter.interpret(stmt))
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
    System.err.println(s"[line: ${error.token.line}]: ${error.message}")
    hadRuntimeError = true
  }

  def report(line: Long, where: String, message: String): Unit = {
    System.err.println(s"[line: ${line}] Error ${where}: ${message}")
    hadError = true
  }
}
