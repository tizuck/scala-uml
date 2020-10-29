package scalameta.stats.dcl

import scalameta.common.RelationshipBaseCollector
import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Class, Operation, Relationship, UMLElement}

import scala.meta.Decl

case class DclRelationshipCollector(definedElements:List[UMLElement],
                                    override val resultingContext: CollectorContext) extends RelationshipBaseCollector

object DclRelationshipCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclRelationshipCollector = {
    val dclRelationsships = dcl match {
      case d : Decl.Val => Some(DclValRelationshipCollector(d))
      case d : Decl.Var => Some(DclVarRelationshipCollector(d))
      case d : Decl.Def => Some(DclDefCollector(d))
      case d : Decl.Type => Some(DclTypeRelationshipCollector(d))
      case _ => None
    }

    //@todo think about pattern matching over a list of uml elements instead of all the optional values
    val wrap = dclRelationsships.getOrElse(new RelationshipBaseCollector{
      override val definedElements: List[UMLElement] = Nil
      override val resultingContext: CollectorContext = context
    })

    new DclRelationshipCollector(wrap.definedElements,wrap.resultingContext)
  }
}
