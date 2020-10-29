package scalameta.stateless

import scalameta.util.CollectorContext

import scala.meta.Type

case class BoundsNameCollector(bounds:Option[String])

object BoundsNameCollector {
  def apply(bounds:Type.Bounds)(implicit context:CollectorContext): BoundsNameCollector = bounds match {
    case Type.Bounds(None, Some(Type.ApplyInfix(Type.Name(lower),Type.Name(":>"),Type.Name(higher)))) =>
      new BoundsNameCollector(Some(s"Bounds<$lower,$higher>"))
    case Type.Bounds(None,Type.Name(lo)) =>
      new BoundsNameCollector(Some(s"LowerBound<$lo>"))
      //@todo for T :> A the behaviour of scala meta is not logical
    case _ => BoundsNameCollector(None)
  }
}
