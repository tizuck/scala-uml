package scalameta.stats.defn

import scalameta.util.{BaseCollector, CollectorContext}
import uml.{Class, Extension, FromTo, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnRepeatedEnumCaseCollector(override val definedElements: List[UMLElement],
                                         override val resultingContext: CollectorContext) extends BaseCollector

object DefnRepeatedEnumCaseCollector {
  def apply(repeatedEnumCase:Defn.RepeatedEnumCase)(implicit context:CollectorContext):
  DefnRepeatedEnumCaseCollector = {
    val caseAsClasses = repeatedEnumCase.cases.foldLeft(List.empty[Class]){
      case (acc,enumCase) =>
       Class(false,enumCase.value,Nil,Nil,Nil,None,Some("case")) :: acc
    }

    caseAsClasses.foldLeft(DefnRepeatedEnumCaseCollector(Nil,context)){
      case (acc,cls) =>
        val relationship =
          Relationship(
            Extension,
            ToFrom,
            RelationshipInfo(None,None,context.thisPointer.get,cls,None,Without)
            ,None
          )
        acc.copy(
          definedElements = cls :: relationship :: acc.definedElements,
          resultingContext = acc.resultingContext.copy(definedTemplates = cls :: acc.resultingContext.definedTemplates)
        )
    }
  }
}
