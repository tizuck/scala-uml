package uml.strategies.rewriting
import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.NamespaceEntry
import uml._

object InsertNamespaceObjectRelsStrat extends RewriteStrategy[List[uml.Class]] {
  override def apply(v1: List[uml.Class]): Strategy = {
    val f: Any => Any = {
      case u@UMLUnit(_, toplevelElements) =>
        val inner =
          v1
            .foldLeft(List.empty[Relationship]) {
              case (acc, cls) =>
                val source = ClassRef(cls.name, cls.namespace)
                val target = PackageRef(cls.namespace.appended(NamespaceEntry(List(cls.name))).asInstanceOf[NamespaceEntry])
                acc
                  .appended(Relationship(Inner, ToFrom, RelationshipInfo(None, None, source, target, None, ToFrom), Nil))
            }
        u.copy(toplevelElements = toplevelElements ++ inner)
      case u@_ => u
    }
    rulef(f)
  }
}
