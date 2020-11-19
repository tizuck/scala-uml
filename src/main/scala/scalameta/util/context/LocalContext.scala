package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.RelationshipElement
import uml.externalReferences.ClassDefRef

case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        currentCompilationUnit : String,
                        cstrOrigin:Option[String] = None,
                        currentImports:Option[List[NamespaceEntry]] = None,
                        currentNamespace:Entry = DefaultNamespace,
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true,
                        externalReferences:List[ClassDefRef] = Nil)

object LocalContext {
  def apply(compilationUnit:String): LocalContext = new LocalContext(currentCompilationUnit = compilationUnit)
}
