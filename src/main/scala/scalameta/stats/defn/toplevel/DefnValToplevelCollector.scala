package scalameta.stats.defn.toplevel

import scalameta.stateless.TypeNameCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, UMLElement}

import scala.meta.{Defn, Pat}

case class DefnValToplevelCollector(override val definedElements: List[UMLElement],
                                    override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnValToplevelCollector {
  def apply(defnVal:Defn.Val)(implicit context:CollectorContext): DefnValToplevelCollector = {
    val classNames = defnVal.pats.foldLeft(List.empty[String]){
      case (acc,Pat.Var(name)) => acc ++ List(name.value)
    }

    val optionRetTypeRep =
      if(defnVal.decltpe.isDefined)
        Some(TypeNameCollector(defnVal.decltpe.get).typeRep)
      else
        None

    val clss = classNames.foldLeft(List.empty[Class]){
      case (acc,cn) =>
        acc ++ List(Class(
        false,
        cn.toUpperCase,
        List(Attribute(None,None,cn,optionRetTypeRep,None)),
          Nil,
          Nil,
          None,
          Some("val")
      ))
    }

    DefnValToplevelCollector(clss,context)
  }
}
