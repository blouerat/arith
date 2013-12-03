package arith

object Interpreter {

  import terms._, Term._

  def apply(t: Term): Either[String, Value] = eval(t).toRight(s"Evaluation error: $t")

  def eval(t: Term): Option[Value] = t match {
    case True => Some(Value.True)
    case False => Some(Value.False)
    case If(t1, t2, t3) => eval(t1).flatMap {
      case Value.True => eval(t2)
      case Value.False => eval(t3)
      case _ => None
    }
    case Zero => Some(Value.Zero)
    case Succ(t) => eval(t).flatMap {
      case n: Nat => Some(Value.Succ(n))
      case _ => None
    }
    case Pred(t) => eval(t).flatMap {
      case Value.Zero => Some(Value.Zero)
      case Value.Succ(n) => Some(n)
      case _ => None
    }
    case IsZero(t) => eval(t).flatMap {
      case Value.Zero => Some(Value.True)
      case Value.Succ(_) => Some(Value.False)
      case _ => None
    }
    case _ => None
  }
}

