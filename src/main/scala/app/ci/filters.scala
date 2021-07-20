package app.ci

import scala.util.matching.Regex

trait Filter {
  def matches(s:String):Boolean
}

sealed case class Exclude(reg:Regex) extends Filter {
  override def matches(s: String): Boolean = reg.matches(s)
}

sealed case class Not(f:Filter) extends Filter {
  override def matches(s: String): Boolean = !f.matches(s)
}

sealed case class And(f1:Filter,f2:Filter) extends Filter {
  override def matches(s: String): Boolean = f1.matches(s) && f2.matches(s)
}

sealed case class Or(f1:Filter,f2:Filter) extends Filter {
  override def matches(s: String): Boolean = f1.matches(s) || f2.matches(s)
}