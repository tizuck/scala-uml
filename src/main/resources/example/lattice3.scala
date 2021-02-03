package ast.arith.scala3

enum ArithExpr :
  case Value(i:int) extends ArithExpr
  case Div(v1:ArithExpr,v2:ArithExpr) extends ArithExpr
  case Mult(v1:ArithExpr,v2:ArithExpr) extends ArithExpr
  case Sub(v1:ArithExpr,v2:ArithExpr) extends ArithExpr