package arith

import scala.util.parsing.combinator._
import terms.Term, Term._

object Parser extends RegexParsers {

  def tmTrue: Parser[True.type] = "true" ^^ (_ => True)
  def tmFalse: Parser[False.type] = "false" ^^ (_ => False)
  def tmIf: Parser[If] = "if" ~ term ~ "then" ~ term ~ "else" ~ term ^^ {
    case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => If(t1, t2, t3)
  }

  def tmZero: Parser[Zero.type] = "0" ^^ (_ => Zero)
  def tmSucc: Parser[Succ] = "succ" ~ term ^^ {
    case _ ~ t => Succ(t)
  }
  def tmPred: Parser[Pred] = "pred" ~ term ^^ {
    case _ ~ t => Pred(t)
  }
  def tmIsZero: Parser[IsZero] = "iszero" ~ term ^^ {
    case _ ~ t => IsZero(t)
  }

  def term: Parser[Term] = tmTrue | tmFalse | tmZero | tmIf | tmSucc | tmPred | tmIsZero

  def apply(input: String): Either[String, Term] = parseAll(term, input) match {
    case Success(t, _) => Right(t)
    case failure: NoSuccess => Left(s"Parsing error: ${failure.msg}")
  }
}

