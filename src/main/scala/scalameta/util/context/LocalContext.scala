package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.{RelationshipElement}

case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        cstrOrigin:Option[String] = None,
                        currentImports:Option[List[NamespaceEntry]] = None,
                        currentNamespace:Entry = DefaultNamespace,
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true)

object LocalContext {
  def apply(): LocalContext = new LocalContext()
}
