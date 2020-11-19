package scalameta.stateless

import scala.meta.Type

case class TypeNameCollector(typeRep:String)

object TypeNameCollector {
  def apply(mtype:scala.meta.Type): TypeNameCollector = {
    mtype match {
      case Type.Var(name) => new TypeNameCollector(name.syntax)
      case Type.Name(name) => new TypeNameCollector(name)
      case Type.Placeholder(bounds) =>
        val optBounds = BoundsNameCollector(bounds).bounds
        if(optBounds.isDefined)
          new TypeNameCollector(optBounds.get)
        else {
          new TypeNameCollector("WildcardType")
        }
      case Type.Apply(tpe,args) =>
        val applyType = TypeNameCollector(tpe).typeRep
        val argTypes = for (arg <- args) yield {
          TypeNameCollector(arg).typeRep
        }
        new TypeNameCollector(s"$applyType<${argTypes.mkString(",")}>")
      case Type.And(lhs,rhs) =>
        val lhsRep = this(lhs)
        val rhsRep = this(rhs)
        TypeNameCollector(s"&<${lhsRep.typeRep},${rhsRep.typeRep}>")
      case Type.Or(lhs, rhs) =>
        val lhsRep = this(lhs)
        val rhsRep = this(rhs)
        TypeNameCollector(s"|<${lhsRep.typeRep},${rhsRep.typeRep}>")
      case Type.With(lhs, rhs) =>
        println(lhs.structure + " " + rhs.structure)
        TypeNameCollector(s"&<${this(lhs).typeRep},${this(rhs).typeRep}>")
    }
  }
}
