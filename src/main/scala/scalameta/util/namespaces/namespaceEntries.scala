package scalameta.util.namespaces

trait Entry {
  self =>
  final def plantUML:String = self match {
    case DefaultNamespace => ""
    case NamespaceEmpty => ""
    case NamespaceEntry(qualifiers, targetType) =>qualifiers.mkString("::") + "."
  }
}

case object NamespaceEmpty extends Entry
case object DefaultNamespace extends Entry

sealed trait TargetType
case object Wildcard extends TargetType
case object Name extends TargetType
case object Package extends TargetType

/**
 * Entry of a namespace holding the qualifier in correct order in the
 * `qualifier` list.
 *
 * `NamespaceEntry(List("a","b","c"))` for example corresponds to
 * the namespace "a.b.c".
 *
 * @param qualifiers name of namespace qualifiers in correct order.
 */
sealed case class NamespaceEntry(qualifiers: List[String],targetType:TargetType = Package) extends Entry {

  def append(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifiers ++ List(qualifier))

  def append(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(qualifiers ++ namespaceEntry.qualifiers)

  def prepend(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(namespaceEntry.qualifiers ++ qualifiers)

  def prepend(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifier :: qualifiers)

  override def toString: String = s"{${qualifiers.mkString("::")},target=$targetType}"
}
