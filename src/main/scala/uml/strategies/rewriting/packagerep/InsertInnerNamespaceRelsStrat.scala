package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.NamespaceEntry
import uml.strategies.rewriting.RewriteStrategy
import uml._

object InsertInnerNamespaceRelsStrat extends RewriteStrategy[List[uml.Class]] {
  override def apply(v1: List[uml.Class]): Strategy = {
    rulef{
      case u@UMLUnit(_,toplevelElements) =>
        val insertedInner = toplevelElements.foldLeft(List.empty[Relationship]){
          case (acc,elem) => elem match {
            case uml.Package(_, _, namespace) =>
              if(v1.exists(c => c.namespace.appended(NamespaceEntry(List(c.name))).equals(namespace))){
                acc.appended(Relationship(
                  Inner,
                  ToFrom,
                  RelationshipInfo(
                    None,
                    None,
                    ClassRef(
                      namespace.asInstanceOf[NamespaceEntry].qualifiers.last,
                      NamespaceEntry(namespace.asInstanceOf[NamespaceEntry].qualifiers.dropRight(1))
                    ),
                    PackageRef(namespace.asInstanceOf[NamespaceEntry]),
                    None,
                    Without,
                    originType = uml.externalReferences.Object
                  ),
                  List(Stereotype("objectdef",Nil))
                ))
              }else {
                acc
              }
            case _ => acc
          }
        }
        u.copy(toplevelElements = toplevelElements ++ insertedInner)

      case u@_ => u
    }
  }
}
