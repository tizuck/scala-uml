package ast.arith

package python {
  trait Mult
}

sealed trait ArithExpr {
  def collect[T](op:ArithExpr => T):List[T]
}

sealed case class Mult(left:ArithExpr,right:ArithExpr) extends ArithExpr {
  val pyMult : python.Mult
}
sealed case class Add(left:ArithExpr,right:ArithExpr) extends ArithExpr
class Sub() extends ArithExpr {
  val left : ArithExpr
  val right : ArithExpr
}