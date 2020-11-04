package scalameta.typeparams

import scalameta.stateless.TypeNameCollector

import scala.meta.Type

case class TypeBoundsCollector(typeRep:Option[String]) {

}

object TypeBoundsCollector {
  def apply(tbounds:Type.Bounds): TypeBoundsCollector = {
      if(tbounds.lo.isEmpty && tbounds.hi.isEmpty){
        new TypeBoundsCollector(None)
      }
      else if(tbounds.lo.isDefined && tbounds.hi.isEmpty){
        new TypeBoundsCollector(
          Some(s"LowerBound<${TypeNameCollector(tbounds.lo.get).typeRep}>")
        )
      }
      else if(tbounds.lo.isEmpty && tbounds.hi.isDefined){
        new TypeBoundsCollector(
          Some(s"HigherBound<${TypeNameCollector(tbounds.hi.get).typeRep}>")
        )
      }
      else TypeBoundsCollector(
        Some(s"Bound<${TypeNameCollector(tbounds.lo.get).typeRep},${TypeNameCollector(tbounds.hi.get).typeRep}>")
      )
  }
}
