package scalameta.util.metatraverser

import scalameta.util.namespaces.NamespaceEntry
import uml.types.Namespace

import scala.meta.{Defn, Pkg, Term, Traverser, Tree}

/**
 * Performs a sceening of a scala AST collecting namespaces and defined templates
 * in that namespace respecting a `relativePackage` position.
 *
 * @param relativePackage package position of the scala ast. `"default"` if in default-package.
 */
class NamespaceTraverser(relativePackage:NamespaceEntry) extends Traverser {
  type NamespaceMap = scala.collection.mutable.Map[NamespaceEntry,Defn]

  private var map : NamespaceMap = scala.collection.mutable.Map.empty[NamespaceEntry,Defn]

  override def apply(tree: Tree): Unit = tree match {
    case Pkg(ref,stats) =>
      val pkgNamespace  = qualName(ref).prepend(relativePackage)
  }

  /**
   * builds the package name
   * @param qual
   * @return
   */
  def qualName(qual:Term.Ref): NamespaceEntry = qual match {
    case Term.Name(str) => NamespaceEntry(List(str))
    case Term.Select(s:Term.Select,Term.Name(str)) => qualName(s).append(str)
    case Term.Select(n1:Term.Name,n2:Term.Name) => NamespaceEntry(n1.value :: n2.value :: Nil)
    case _ => throw new IllegalStateException(s"unexpected Package name:$qual")
  }
}
