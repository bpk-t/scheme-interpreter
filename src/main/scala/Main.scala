import executor.Executor
import parser.Parser
import printer.Printer
import token.Tokenizer

object Main extends App {

  import ast._
  val tokenizer = new Tokenizer

  /*
  println(tokenizer.tokenize("(+ 1 2)"))
  println(tokenizer.tokenize("(add 102 300)"))
  println(tokenizer.tokenize("(add 102  300)"))
  println(tokenizer.tokenize("(add  102 300)"))
  */
  val parser = new Parser

  /*
  println(parser.parse(Seq("123")))
  println(parser.parse(Seq("\"ABC\"")))
  println(parser.parse(Seq("(", "1", ")")))
  println(parser.parse(Seq("(", "1", "2", "3", ")")))
  println(parser.parse(Seq("(", "1", "(", "2", ")", "3", ")")))
  */

  val printer = new Printer
  val executor = new Executor

  /*
  printer.printExpression(IntValue(100))
  printer.printExpression(StringValue("abc"))
  printer.printExpression(SListImpl(IntValue(1),SNil))
  printer.printExpression(SListImpl(IntValue(1),SListImpl(IntValue(2),SListImpl(IntValue(3),SNil))))

  printer.printExpression(
    SListImpl(IntValue(1),SListImpl(SListImpl(IntValue(2),SNil),SListImpl(IntValue(3),SNil)))
  )

  val expression = SListImpl(
    Symbol("+"),
    SListImpl(
      IntValue(100),
      SListImpl(
        IntValue(200),
        SNil
      )
    )
  )
  */


  val start = System.currentTimeMillis()
  val str =
  """
    (+
      (+ 1 1)
      (- 3 1)
      (* 2 1)
      (/ 4 2)
      (/ 4 2)
      (/ 4 2)
      (/ 4 2)
      (/ 4 2)
      (+
        (* 1 1)
        (* 1 1)
      )
    )
  """


  for {
    _ <- (0 until 1000)
  } yield {
    for {
      tokenList <- tokenizer.tokenize(str)
      ast <- parser.parse(tokenList)
      appliedAst <- executor.execute(ast)
    } yield {
      //print("exec : ")
      //printer.printExpression(appliedAst)
    }
  }

  println(s"elap = ${System.currentTimeMillis() - start}ms")
}
