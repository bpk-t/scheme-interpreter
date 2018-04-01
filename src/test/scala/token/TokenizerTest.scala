package token

import org.scalatest.FunSuite

class TokenizerTest extends FunSuite {
  val tokenizer = new Tokenizer

  test("Tokenizer.tokenize empty string") {
    val ret = tokenizer.tokenize("")
    assert(ret.isLeft)
  }

  test("Tokenizer.tokenize 1token") {
    val ret = tokenizer.tokenize("123")
    assert(ret.isRight)
    assert(ret.right.get === Seq("123"))
  }

  test("Tokenizer.tokenize 2token, space split") {
    val ret = tokenizer.tokenize("123 456")
    assert(ret.isRight)
    assert(ret.right.get === Seq("123", "456"))
  }

  test("Tokenizer.tokenize 2token, 2space split") {
    val ret = tokenizer.tokenize("123  456")
    assert(ret.isRight)
    assert(ret.right.get === Seq("123", "456"))
  }

  test("Tokenizer.tokenize 2token, tab split") {
    val ret = tokenizer.tokenize("123 456")
    assert(ret.isRight)
    assert(ret.right.get === Seq("123", "456"))
  }

  test("Tokenizer.tokenize 2token, new line split") {
    val ret = tokenizer.tokenize(
      """
        ABC
        456
      """
    )
    assert(ret.isRight)
    assert(ret.right.get === Seq("ABC", "456"))
  }

  test("Tokenizer.tokenize brackets split") {
    val ret = tokenizer.tokenize("()")
    assert(ret.isRight)
    assert(ret.right.get === Seq("(", ")"))
  }

  test("Tokenizer.tokenize brackets split and field") {
    val ret = tokenizer.tokenize("(+ 1 2)")
    assert(ret.isRight)
    assert(ret.right.get === Seq("(", "+", "1", "2", ")"))
  }

  test("Tokenizer.tokenize nest brackets split and field") {
    val ret = tokenizer.tokenize("(+ (- 0 10) 2)")
    assert(ret.isRight)
    assert(ret.right.get === Seq("(", "+", "(", "-", "0", "10", ")", "2", ")"))
  }
}
