package scalameta.util.namespaces

trait Entry

case object NamespaceEmpty extends Entry

/**
 * Entry of a namespace holding the qualifier in correct order in the
 * `qualifier` list.
 *
 * `NamespaceEntry(List("a","b","c"))` for example corresponds to
 * the namespace "a.b.c".
 *
 * @param qualifiers name of namespace qualifiers in correct order.
 */
sealed case class NamespaceEntry(qualifiers: List[String]) extends Entry {

  def append(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifiers ++ List(qualifier))

  def append(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(qualifiers ++ namespaceEntry.qualifiers)

  def prepend(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(namespaceEntry.qualifiers ++ qualifiers)

  def prepend(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifier :: qualifiers)
}
