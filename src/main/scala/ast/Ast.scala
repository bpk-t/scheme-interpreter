package ast

sealed trait SExpression {
  def toSeq(): Seq[SExpression] = {
    def toSeq_(expression: SExpression): Seq[SExpression] = {
      expression match {
        case SListImpl(car, cdr) =>
          car +: toSeq_(cdr)
        case SNil => Nil
        case x => Seq(x)
      }
    }
    toSeq_(this)
  }
}
sealed trait Value extends SExpression
sealed trait Number extends Value

case class IntValue(value: Int) extends Number
case class DoubleValue(value: Double) extends Number
case class StringValue(value: String) extends Value

case class Symbol(name: String) extends SExpression

sealed trait SList extends SExpression
case class SListImpl(
  car: SExpression,
  cdr: SList
) extends SList

case object SNil extends SList