package scalameta.stats.dcl


import scalameta.relationships.dcl.DclDefCollector
import scalameta.util.{BaseCollector, CollectorContext}
import uml.{Class, Operation, Relationship, UMLElement}

import scala.meta.Decl

case class DclCollector(definedElements:List[UMLElement],
                        override val resultingContext: CollectorContext) extends BaseCollector

object DclCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclCollector = {
    val dclRelationsships = dcl match {
      case d : Decl.Val => Some(DclValCollector(d))
      case d : Decl.Var => Some(DclVarCollector(d))
      case d : Decl.Def => Some(DclDefCollector(d))
      case d : Decl.Type => Some(DclTypeCollector(d))
      case _ => None
    }

    //@todo think about pattern matching over a list of uml elements instead of all the optional values
    val wrap = dclRelationsships.getOrElse(new BaseCollector {
      override val definedElements: List[UMLElement] = Nil
      override val resultingContext: CollectorContext = context
    })

    new DclCollector(wrap.definedElements,wrap.resultingContext)
  }
}
