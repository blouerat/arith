package arith.terms

sealed trait Term
object Term {
  case object True extends Term
  case object False extends Term
  case class If(t1: Term, t2: Term, t3: Term) extends Term

  case object Zero extends Term
  case class Succ(t: Term) extends Term
  case class Pred(t: Term) extends Term
  case class IsZero(t: Term) extends Term
}

sealed trait Value
sealed trait Nat extends Value
object Value {
  case object True extends Value
  case object False extends Value
  case object Zero extends Nat
  case class Succ(n: Nat) extends Nat
}