package printer

import ast._

class Printer {
  def printExpression(expression: SExpression): Unit = {
    def printExpression_(expression: SExpression, prev: Option[SExpression]): Unit = {
      expression match {
        case list@SListImpl(car, cdr) =>
          if (prev == None) {
            print("(")
          }
          printExpression_(car, None)
          if (cdr != SNil) {
            print(" ")
          }
          printExpression_(cdr, Some(list))
        case SNil =>
          print(")")
        case Symbol(name) =>
          print(name)
        case IntValue(n) =>
          print(n)
        case DoubleValue(n) =>
          print(n)
        case StringValue(s) =>
          print(s)
      }
    }
    printExpression_(expression, None)
    println()
  }
}
