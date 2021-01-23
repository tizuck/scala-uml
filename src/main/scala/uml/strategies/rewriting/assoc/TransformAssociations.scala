package uml.strategies.rewriting.assoc

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{Attribute, ClassRef, FromTo, Relationship, ToFrom, UMLUnit}
import uml.strategies.rewriting.RewriteStrategy

object TransformAssociations extends RewriteStrategy[List[Relationship]]{
  override def apply(v1: List[Relationship]): Strategy = {
    val f: Any => Any = u => u match {
      case u:UMLUnit => u.copy(toplevelElements = u.toplevelElements.map{
        case c:uml.Class =>
          val allMatchingRelationshipsFromTo = v1.collect(r => if( r.relationshipDirection.equals(FromTo) && (r.relationshipInfo.from match {
            case cr:ClassRef => c.name.equals(cr.name) && c.namespace.equals(cr.namespace)
          })) Some(r) else None).flatten

          val classFromTo = allMatchingRelationshipsFromTo.foldLeft(c){
            case (acc,rinner) =>
              val targetType = rinner.relationshipInfo.to match {
                case cr:ClassRef => cr.name
              }
              val targetMultiplicity = rinner.relationshipInfo.sourceMultiplicity
              val targetName = rinner.relationshipInfo.relationshipIdentifier.getOrElse("unknownName")
              acc.copy(attributes = acc.attributes.appended(Attribute(None,None,targetName,Some(targetType),Nil,None)))
          }

          val allMatchingRelationshipsToFrom = v1.collect(r => if( r.relationshipDirection.equals(ToFrom) && (r.relationshipInfo.to match {
            case cr:ClassRef => c.name.equals(cr.name) && c.namespace.equals(cr.namespace)
          })) Some(r) else None).flatten

          allMatchingRelationshipsFromTo.foldLeft(c){
            case (acc,rinner) =>
              val targetType = rinner.relationshipInfo.to match {
                case cr:ClassRef => cr.name
              }
              //@todo add multiplicity to attributes
              val targetMultiplicity = rinner.relationshipInfo.sourceMultiplicity
              val targetName = rinner.relationshipInfo.relationshipIdentifier.getOrElse("unknownName")
              acc.copy(attributes = acc.attributes.appended(Attribute(None,None,targetName,Some(targetType),Nil,None)))
          }
        case u@_ => u
      })
      case u@_ => u
    }

    rulef(f)
  }

  private def interpreteRelationshipId(relId:String):String = {
    val name = relId.substring(0,relId.indexOf("<<bind"))
    val generics =
      relId
        .substring(relId.indexOf("<<bind"),relId.indexOf(">>"))
        .drop(2)
        .dropRight(2)
        .split(",")
        .map{
          s => s.split("->")
        }

  }
}
