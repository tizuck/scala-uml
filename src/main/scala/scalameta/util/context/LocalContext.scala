package scalameta.util.context

import java.io.IOException

import pureconfig.ConfigSource
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.RelationshipElement
import uml.externalReferences.ClassDefRef
import pureconfig._
import pureconfig.generic.auto._



case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        currentCompilationUnit : String,
                        cstrOrigin:Option[String] = None,
                        currentImports:Option[List[NamespaceEntry]] = None,
                        currentNamespace:Entry = DefaultNamespace,
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true,
                        externalReferences:List[ClassDefRef] = Nil,
                        opReps:Ops)

object LocalContext {
  def apply(compilationUnit:String): LocalContext = {
    val loadedOpsConfig = ConfigSource.file("src/main/resources/operators.conf").load[Ops]
    val ops = loadedOpsConfig match {
      case Right(value) => value
      case l@_ =>  throw new IOException(s"src/main/resources/operators.conf is corrupt with output: $l.")
    }
    new LocalContext(
      currentCompilationUnit = compilationUnit,
      opReps = ops
    )
  }
}
