package uml.strategies.rewriting
import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEntry}
import uml.{ClassRef, Inner, PackageRef, Relationship, RelationshipInfo, ToFrom, UMLUnit}

object InsertNamespaceObjectRelsStrat extends RewriteStrategy[List[uml.Class]] {
  override def apply(v1: List[uml.Class]): Strategy = {
    val f: Any => Any = u => u match {
      case u@UMLUnit(_,toplevelElements) =>
        val inner =
          v1
            .foldLeft(List.empty[Relationship]){
              case (acc,cls) =>
                val source = ClassRef(cls.name,cls.namespace)
                val target = PackageRef(cls.namespace.appended(NamespaceEntry(List(cls.name))).asInstanceOf[NamespaceEntry])
                acc
                .appended(Relationship(Inner,ToFrom,RelationshipInfo(None,None,source,target,None,ToFrom),Nil))
            }
        u.copy(toplevelElements = toplevelElements ++ inner)
      case u@_ => u
    }
    rulef(f)
  }
}
