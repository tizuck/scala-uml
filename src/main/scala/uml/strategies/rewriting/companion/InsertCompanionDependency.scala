package uml.strategies.rewriting.companion

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{Annotation, ClassRef, ConcreteClass, FromTo, Relationship, RelationshipInfo, Stereotype, UMLUnit, Without}
import uml.strategies.rewriting.RewriteStrategy

object InsertCompanionDependency extends RewriteStrategy[List[(uml.Class,Boolean)]] {
  override def apply(v1: List[(uml.Class, Boolean)]): Strategy = {
    val f: Any => Any = u => u match {
      case u:UMLUnit => v1.foldLeft(u){
        case (acc,(c,b)) if b => acc.copy(toplevelElements = acc.toplevelElements.appended(
          Relationship(Annotation,FromTo,RelationshipInfo(None,None,ConcreteClass(c),ClassRef("$"+c.name,c.namespace),None,Without),List(Stereotype("companion",Nil)))
        ))
        case (acc,_) => acc
      }
      case o@_ => o
    }
    rulef(f)
  }
}
