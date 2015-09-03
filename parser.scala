package org.cvogt
import org.parboiled2._

object TimeExpressionParserTest extends App {

  println( parseTimeExpression("Mar 2nd") )
  println( parseTimeExpression("Jan\n1st") )

  println( parseTimeExpression("some bullshit") )

}

sealed trait Month
case object January extends Month
case object February extends Month
case object March extends Month

case class Date(monthOfYear: Month, dayOfMonth: Int)

class parseTimeExpression private(val input: ParserInput) extends Parser {
  def All = rule{ TimeExpression ~ EOI }

  def TimeExpression = rule{ MonthOfYear ~ separator ~ DayOfMonth ~> Date }

  def separator = rule { "\t" | " " | "\r" | "\n" | "\u000C" }

  def DayOfMonth = rule{ capture("1st" | "2nd") ~> ((_: String) match {
    case "1st" => 1
    case "2nd" => 2
  } )}

  def MonthOfYear = rule{ capture("Jan" | "Feb" | "Mar") ~> ((_: String) match {
    case "Jan" => January
    case "Feb" => February
    case "Mar" => March
  })}

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

object parseTimeExpression{
  def apply(expr: String) = new parseTimeExpression(expr).All.run()
}
