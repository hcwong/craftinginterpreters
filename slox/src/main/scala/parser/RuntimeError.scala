package parser

import tokens.Token

case class RuntimeError(token: Token, message: String) extends RuntimeException
