package parser

import ast._
import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  val parser = new Parser

  test("Parser.parse Seq empty") {
    val ret = parser.parse(Seq.empty)
    assert(ret.isLeft)
  }
  test("Parser.parse IntValue") {
    val ret = parser.parse(Seq("123"))
    assert(ret.isRight)
    assert(ret.right.get === IntValue(123))
  }

  test("Parser.parse IntValue plus") {
    val ret = parser.parse(Seq("+123"))
    assert(ret.isRight)
    assert(ret.right.get === IntValue(123))
  }

  test("Parser.parse IntValue minus") {
    val ret = parser.parse(Seq("-123"))
    assert(ret.isRight)
    assert(ret.right.get === IntValue(-123))
  }

  test("Parser.parse DoubleValue") {
    val ret = parser.parse(Seq("10.0"))
    assert(ret.isRight)
    assert(ret.right.get === DoubleValue(10d))
  }

  test("Parser.parse DoubleValue minus") {
    val ret = parser.parse(Seq("-2.5"))
    assert(ret.isRight)
    assert(ret.right.get === DoubleValue(-2.5d))
  }

  test("Parser.parse StringValue") {
    val ret = parser.parse(Seq("\"ABC\""))
    assert(ret.isRight)
    assert(ret.right.get === StringValue("ABC"))
  }

  test("Parser.parse StringValue empty") {
    val ret = parser.parse(Seq("\"\""))
    assert(ret.isRight)
    assert(ret.right.get === StringValue(""))
  }

  test("Parser.parse Symbol") {
    val ret = parser.parse(Seq("+"))
    assert(ret.isRight)
    assert(ret.right.get === Symbol("+"))
  }

  test("Parser.parse List 1 arg") {
    val ret = parser.parse(Seq("(", "1", ")"))
    assert(ret.isRight)
    assert(ret.right.get === SListImpl(IntValue(1), SNil))
  }

  test("Parser.parse List 2 arg") {
    val ret = parser.parse(Seq("(", "1", "2", ")"))
    assert(ret.isRight)
    assert(ret.right.get === SListImpl(IntValue(1), SListImpl(IntValue(2), SNil)))
  }

  test("Parser.parse List nest") {
    val ret = parser.parse(Seq("(", "(", "1", ")", "2", ")"))
    assert(ret.isRight)
    assert(ret.right.get === SListImpl(
      SListImpl(
        IntValue(1),
        SNil
      ),
      SListImpl(
        IntValue(2),
        SNil
      ))
    )
  }
}
