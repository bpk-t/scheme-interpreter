package token

import scala.collection.mutable.ListBuffer

class Tokenizer {
  sealed trait State
  case object InitState extends State
  case object StringState extends State

  def tokenize(input: String): Either[String, Seq[String]] = {
    val tokenList = new ListBuffer[String]
    var currentToken: String = ""

    def addToken(c: Char): Unit = {
      currentToken += c
    }
    def addTokenList(): Unit = {
      val trimmed = currentToken.trim
      if (!trimmed.isEmpty) {
        tokenList.append(currentToken)
        currentToken = ""
      }
    }

    var currentState: State = InitState
    input.foreach { c =>
      c match {
        case '(' | ')' =>
          addTokenList()
          addToken(c)
          addTokenList()
        case '\r' | '\n' | ' ' | '\t' =>
          if (currentState != StringState) {
            addTokenList()
          }
        case '"' =>
          addToken(c)
          currentState match {
            case InitState =>
              currentState = StringState
            case StringState =>
              addTokenList()
              currentState = InitState
          }
        case x =>
          addToken(x)
      }
    }
    addTokenList()

    if (tokenList.isEmpty) {
      Left("token list is empty.")
    } else {
      Right(tokenList.toSeq) // match errorになるのでtoSeqが必要
    }
  }
}
