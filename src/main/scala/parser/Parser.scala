package parser

import ast._

class Parser {
  private val IntMatch = """([+-]?[0-9]+)""".r
  private val DoubleMatch = """([+-]?[.0-9]+)""".r
  private val StringMatch = """"(.*)"""".r

  case class ParseException(message: String) extends Exception

  sealed trait Operation
  case object Atom extends Operation
  case class InnerList(count: Int) extends Operation
  case class MakeList(count: Int) extends Operation

  def parse(list: Seq[String]): Either[String, SExpression] = {
    var tokenList = list
    def nextToken(): String = {
      tokenList match {
        case x :: xs =>
          tokenList = xs
          x
        case Nil =>
          throw new ParseException("tokenList is empty.")
      }
    }

    var valueStack = List.empty[SExpression]
    def pushValue(expression: SExpression): Unit = valueStack = expression :: valueStack
    def popValue(): SExpression = valueStack match {
      case x :: xs =>
        valueStack = xs
        x
      case Nil =>
        throw new ParseException("valueStack is empty.")
    }

    var opStack = List[Operation](Atom)
    def pushOp(op: Operation): Unit = opStack = op :: opStack
    def popOp(): Operation = opStack match {
      case x :: xs =>
        opStack = xs
        x
      case Nil =>
        throw new ParseException("opStack is empty.")
    }

    def parse_(): Unit = {
      while(!tokenList.isEmpty || !opStack.isEmpty) {
        popOp() match {
          case Atom =>
            nextToken() match {
              case "(" =>
                pushOp(InnerList(0))
              case ")" =>
                throw new ParseException("syntax error")
              case IntMatch(value) =>
                pushValue(IntValue(value.toInt))
              case DoubleMatch(value) =>
                pushValue(DoubleValue(value.toDouble))
              case StringMatch(value) =>
                pushValue(StringValue(value))
              case x =>
                pushValue(Symbol(x))
            }
          case InnerList(n) =>
            //println(s"InnerList n = ${n}, opStack = ${opStack}")
            nextToken() match {
              case "(" =>
                pushOp(InnerList(n + 1))
                pushOp(InnerList(0))
              case ")" =>
                //println("call )")
                pushOp(MakeList(n))
              case IntMatch(value) =>
                pushValue(IntValue(value.toInt))
                pushOp(InnerList(n + 1))
              case DoubleMatch(value) =>
                pushValue(DoubleValue(value.toDouble))
                pushOp(InnerList(n + 1))
              case StringMatch(value) =>
                pushValue(StringValue(value))
                pushOp(InnerList(n + 1))
              case x =>
                pushValue(Symbol(x))
                pushOp(InnerList(n + 1))
            }
          case MakeList(n) =>
            //println(s"MakeList n = ${n}, valueStack = ${valueStack}")
            pushValue(
              (0 until n).map(_ => popValue())
                .reverse
                .foldRight[SList](SNil) {
                case (x, acc) =>
                  //println(s"[foldRight] x = ${x}, acc = ${acc}")
                  SListImpl(x, acc)
              }
            )
        }
      }
    }

    try {
      parse_()
      if (valueStack.size == 1) {
        Right(valueStack.head)
      } else {
        Left("valueStack.size is not 1")
      }
    } catch {
      case ParseException(message) =>
        Left(message)
    }
  }
}
