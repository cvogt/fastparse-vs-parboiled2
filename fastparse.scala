package org.cvogt.parser_experiments.fastparse
import fastparse.all._

object TimeExpressionParserTest extends App {

  println( parseTimeExpression("Mar 2nd") )
  println( parseTimeExpression("Jan\n1st") )

  println( parseTimeExpression("some bullshit") )
  println( parseTimeExpression("Mar 2ndcrap") )

}

sealed trait Month
case object January extends Month
case object February extends Month
case object March extends Month

case class Date(monthOfYear: Month, dayOfMonth: Int)

object TimeExpressionParser extends{
  // careful, always use `lazy` vals here. Otherwise null pointer exception. General Scala problem.
  lazy val Grammar = Start ~ TimeExpression ~ End

  lazy val TimeExpression = ( MonthOfYear ~ separator ~ DayOfMonth ).map( (Date.apply _).tupled )

  lazy val separator = "\t" | " " | "\r" | "\n" | "\u000C"

  lazy val DayOfMonth = ("1st" | "2nd").!.map{
    case "1st" => 1
    case "2nd" => 2
  }

  lazy val MonthOfYear = ("Jan" | "Feb" | "Mar").!.map{
    case "Jan" => January
    case "Feb" => February
    case "Mar" => March
  }
}

object parseTimeExpression{
  def apply(expr: String) = TimeExpressionParser.Grammar.parse(expr)
}
