package executor

import ast._

class Executor {
  case class ExecutorException(message: String) extends Exception(message)

  def execute(expression: SExpression): Either[String, SExpression] = {
    def arithmetic(arguments: Seq[SExpression])(fold: (Option[Double], Double) => Option[Double]): Number = {
      if (arguments.forall(_.isInstanceOf[Number])) {
        if (arguments.find(_.isInstanceOf[DoubleValue]).isDefined) {
          DoubleValue(
            arguments.map {
              case IntValue(n) => n.toDouble
              case DoubleValue(n) => n
              case _ => ???
            }.foldLeft[Option[Double]](None)(fold).get // TODO
          )
        } else {
          IntValue(
            arguments.map {
              case IntValue(n) => n.toDouble
              case _ => ???
            }.foldLeft[Option[Double]](None)(fold).get.toInt // TODO
          )
        }
      } else {
        throw new ExecutorException("It contains something other than number type.")
      }
    }

    def execute_(expression: SExpression): SExpression = {
      expression match {
        case list:SListImpl =>
          list.toSeq() match {
            case x :: xs =>
              x match {
                case Symbol(name) =>
                  val applyList = xs.map(execute_)
                  name match {
                    case "+" =>
                      arithmetic(applyList)((accOpt, x) => accOpt.fold(Some(x))(acc => Some(acc + x)))
                    case "-" =>
                      arithmetic(applyList)((accOpt, x) => accOpt.fold(Some(x))(acc => Some(acc - x)))
                    case "*" =>
                      arithmetic(applyList)((accOpt, x) => accOpt.fold(Some(x))(acc => Some(acc * x)))
                    case "/" =>
                      arithmetic(applyList)((accOpt, x) => accOpt.fold(Some(x))(acc => Some(acc / x)))
                    case _=> ???
                  }
                case list: SListImpl =>
                  execute_(list)
                case x => x
              }
            case Nil =>
              SNil
          }
        case x => x
      }
    }

    try {
      Right(
        execute_(expression)
      )
    } catch {
      case ExecutorException(message) =>
        Left(message)
    }
  }
}
