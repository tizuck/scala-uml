package scalameta.util.context

import uml.types.Namespace
import uml.{NamedElement, RelateableElement, RelationshipElement}

case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        definedTemplates: List[NamedElement with RelateableElement] = List.empty,
                        cstrOrigin:Option[String],
                        currentImports:Option[List[Namespace]],
                        currentNamespace:Namespace = "default",
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true)

object LocalContext {
  def apply(): LocalContext = new LocalContext(None, Nil, None, None)
}
