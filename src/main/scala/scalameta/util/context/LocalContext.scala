package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.types.Namespace
import uml.{NamedElement, RelateableElement, RelationshipElement}

case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        definedTemplates: List[NamedElement with RelateableElement] = List.empty,
                        cstrOrigin:Option[String],
                        currentImports:Option[List[NamespaceEntry]],
                        currentNamespace:Entry = DefaultNamespace,
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true)

object LocalContext {
  def apply(): LocalContext = new LocalContext(None, Nil, None, None)
}
