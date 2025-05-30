package LoxApp

import scala.io.StdIn.readLine
import scanner.Scanner

object LoxApp {
  private var hadError: Boolean = false

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
    println(Scanner(source).scanTokens())
  }

  def error(line: Long, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Long, where: String, message: String): Unit = {
    System.err.println(s"[line: ${line}] Error ${where}: ${message}")
    hadError = true
  }
}
