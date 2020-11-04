package scalameta.stats.init


import scalameta.stateless.TypeNameCollector
import scalameta.util.StateChangingCollector
import scalameta.util.context.CollectorContext
import uml._

import scala.meta.{Defn, Init, Term}

case class InitCollector(inheritance:Relationship,
                         initValues:List[Attribute],
                         override val resultingContext: CollectorContext) extends StateChangingCollector

object InitCollector {
  def apply(init:Init)(implicit context:CollectorContext): InitCollector = {
    val extendedType = TypeNameCollector(init.tpe)

    //Define Template for inner call in case it has not been defined before
    val newContext = if(context.definedTemplates.forall( (n:NamedElement) => !n.identifier.equals(extendedType.typeRep) )) {
      context.copy(definedTemplates =  Class(false,extendedType.typeRep,List.empty,List.empty,List.empty,None,None) :: context.definedTemplates)
    } else {context}

    val initAttrs = init.argss.foldLeft((0,List.empty[Attribute])){
      case (acc,args) =>
        val attr = args.foldLeft((acc._1,List.empty[Attribute])){
          case (acc2,arg) =>
            (acc2._1 + 1,Attribute(None,None,s"initid_${acc2._1} = ${arg.syntax}",None,Some("cstrinit")) :: acc2._2)
        }

        (attr._1,acc._2 ++ attr._2)
    }

    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(None,None, newContext.definedTemplates.find((n:NamedElement) => n.identifier.equals(extendedType.typeRep)).get,context.thisPointer.get,None,Without),None)
    new InitCollector(inheritance,initAttrs._2,newContext)

    //@todo build in the init inheritance
  }
}
