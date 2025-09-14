package runtime

case class ReturnException(
    returnValue: Any
) extends Exception
