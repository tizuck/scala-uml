package scalameta.stateless

import scalameta.util.context.CollectorContext

import scala.meta.Type

case class TypeNameCollector(typeRep:String)

object TypeNameCollector {
  //@todo precedences for type is needed for correct output
  def apply(mtype:scala.meta.Type)(implicit context:CollectorContext): TypeNameCollector = {
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
      case Type.ApplyInfix(lhs,op,rhs) =>
        val lhsRep = this(lhs).typeRep
        val rhsRep = this(rhs).typeRep
        val opRep = context.localCon.opReps.ops.find(oe => oe.op.equals(op.value)).map(_.rep)
        TypeNameCollector(s"${opRep.getOrElse(op.value)}<$lhsRep,$rhsRep>")
      case Type.Function(params,res) =>
        val paramsReps = params.map(t => TypeNameCollector(t).typeRep)
        val paramsRep =
          if(params.size > 1) {
            s"Tuple${params.size}<${paramsReps.mkString(",")}>"
          } else {
            s"${paramsReps.head}"
          }

        val resRep = TypeNameCollector(res).typeRep
        TypeNameCollector(s"Func<$paramsRep,$resRep>")
    }
  }
}
