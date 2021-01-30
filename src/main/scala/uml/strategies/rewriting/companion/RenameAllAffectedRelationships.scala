package uml.strategies.rewriting.companion

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{ClassRef, ConcreteClass, FromTo, PackageBodyElement, PackageRef, Relationship, RelationshipElement, ToFrom, TopLevelElement, UMLElement, UMLUnit}
import uml.strategies.rewriting.RewriteStrategy

object RenameAllAffectedRelationships extends RewriteStrategy[List[(uml.Class,Boolean)]] {
  override def apply(v1: List[(uml.Class, Boolean)]): Strategy = {
    val f : Any => Any = u => u match {
      case u:UMLUnit =>
        u.copy(toplevelElements = updateInnerElements(u.toplevelElements,v1).asInstanceOf[List[TopLevelElement]])
      case p:uml.Package =>
        p.copy(packageBodyElements = updateInnerElements(p.packageBodyElements,v1).asInstanceOf[List[PackageBodyElement]])
      case u@_ => u
    }
    rulef(f)
  }

  private def updateInnerElements(topLevelElements:List[UMLElement], v1: List[(uml.Class, Boolean)]) = {
    topLevelElements
      .map {
        case r: Relationship =>
          //If object is found, see if this object is a companion object
          //and if so, update the name of the from identifier in the relationship
          if(r.stereotype.exists(s => s.name.equals("objectdef"))) {
            println(r.structure)
          }
          if (r.relationshipDirection.equals(FromTo) &&
            r.relationshipInfo.originType.equals(uml.externalReferences.Object)) {
            updateRelationship(v1, r, r.relationshipInfo.from)
          }
          else if (r.relationshipDirection.equals(ToFrom) &&
            r.relationshipInfo.originType.equals(uml.externalReferences.Object)) {
            updateRelationship(v1, r, r.relationshipInfo.to)
          }
          else {
            r
          }
        case u@_ => u
      }
  }

  private def updateRelationship(v1: List[(uml.Class, Boolean)], r: Relationship, relElem:RelationshipElement) : Relationship = {
    val nameOfObject = relElem match {
      case ConcreteClass(cls) => Some(ClassRef(cls.name, cls.namespace))
      case c: ClassRef => Some(c)
      case _ => None
    }
    //We found a matching relationship
    if (v1.exists(tp => tp._2 &&
      nameOfObject.isDefined &&
      tp._1.name.equals(nameOfObject.get.name) &&
      tp._1.namespace.equals(nameOfObject.get.namespace))) {
      relElem match {
        case c: ConcreteClass =>
          if(relElem.equals(r.relationshipInfo.from)) {
            r.copy(relationshipInfo =
              r.relationshipInfo.copy(from =
                c.copy(cls =
                  c.cls.copy(name =
                    "$" + c.cls.name)))
            )
          } else {
            r.copy(relationshipInfo =
              r.relationshipInfo.copy(to =
                c.copy(cls =
                  c.cls.copy(name =
                    "$" + c.cls.name)))
            )
          }
        case c: ClassRef =>
          if(relElem.equals(r.relationshipInfo.from)) {
            r.copy(relationshipInfo =
              r.relationshipInfo.copy(from =
                c.copy(name =
                  "$" + c.name))
            )
          } else {
            r.copy(relationshipInfo =
              r.relationshipInfo.copy(to =
                c.copy(name =
                  "$" + c.name))
            )
          }
      }
    } else if(nameOfObject.isEmpty && (relElem match {
      case _:PackageRef => true
      case _ => false
    })) {
      if(relElem.equals(r.relationshipInfo.from)) {
        r.copy(
          relationshipInfo = r.relationshipInfo.copy(
            to = r.relationshipInfo.to match {
              case c:ClassRef => c.copy(name = "$" + c.name)
              case c:ConcreteClass => c.copy(c.cls.copy(name = "$" + c.cls.name))
              case p:PackageRef => p
            }
          )
        )
      } else {
        println("Here")
        r.copy(
          relationshipInfo = r.relationshipInfo.copy(
            from = r.relationshipInfo.from match {
              case c:ClassRef => c.copy(name = "$" + c.name)
              case c:ConcreteClass => c.copy(c.cls.copy(name = "$" + c.cls.name))
              case p:PackageRef => p
            }
          )
        )
      }
    } else {
      r
    }
  }
}
