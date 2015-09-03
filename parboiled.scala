package org.cvogt.parser_experiments.parboiled2
import org.parboiled2._

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

class TimeExpressionParser(val input: ParserInput) extends Parser {
  // careful, always use `lazy` vals here. Otherwise null pointer exception. General Scala problem.
  lazy val Grammar = rule{ TimeExpression ~ EOI }

  lazy val TimeExpression = rule{ MonthOfYear ~ separator ~ DayOfMonth ~> Date }

  lazy val separator = rule { "\t" | " " | "\r" | "\n" | "\u000C" }

  lazy val DayOfMonth = rule{ capture("1st" | "2nd") ~> ((_: String) match {
    case "1st" => 1
    case "2nd" => 2
  } )}

  lazy val MonthOfYear = rule{ capture("Jan" | "Feb" | "Mar") ~> ((_: String) match {
    case "Jan" => January
    case "Feb" => February
    case "Mar" => March
  })}
}

object parseTimeExpression{
  def apply(expr: String) = new TimeExpressionParser(expr).Grammar.run()
}
