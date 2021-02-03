package ast

import python.ast.arith._

trait ArithExpr {
  def foldLeft[T,C](start:T)(op:(T,ArithExpr) => T):T
}

object ArithExpr {
  def apply(arth:String):ArithExpr = Foo()
}

sealed case class Add(v1:ArithExpr,v2:ArithExpr) extends ArithExpr {
  override def foldLeft[T, C](start: T)(op: (T, ArithExpr) => T): T = {
    val v1Fold = v1.foldLeft(start)(op)
    val v2Fold = v2.foldLeft(v1Fold)(op)
    op(v2Fold,this)
  }
}
sealed case class Sub(v1:ArithExpr,v2:ArithExpr) extends ArithExpr {
  override def foldLeft[T, C](start: T)(op: (T, ArithExpr) => T): T = {
    val v1Fold = v1.foldLeft(start)(op)
    val v2Fold = v2.foldLeft(v1Fold)(op)
    op(v2Fold,this)
  }

}
sealed case class Mult(v1:ArithExpr,v2:ArithExpr) extends ArithExpr {
  override def foldLeft[T, C](start: T)(op: (T, ArithExpr) => T): T = {
    val v1Fold = v1.foldLeft(start)(op)
    val v2Fold = v2.foldLeft(v1Fold)(op)
    op(v2Fold,this)
  }

}
sealed case class Div(v1:ArithExpr,v2:ArithExpr) extends ArithExpr {
  override def foldLeft[T, C](start: T)(op: (T, ArithExpr) => T): T = {
    val v1Fold = v1.foldLeft(start)(op)
    val v2Fold = v2.foldLeft(v1Fold)(op)
    op(v2Fold,this)
  }

}
sealed case class Value(v:Int) extends ArithExpr {
  override def foldLeft[T, C](start: T)(op: (T, ArithExpr) => T): T = {
    op(start,this)
  }
}


