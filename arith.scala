package arith

object Arith {
  def run(input: String): Unit = {
    val eval = Parser(input).right.flatMap(t => Interpreter(t))
    val result = eval.fold(identity, t => s"Result: $t")
    println(result)
  }
}