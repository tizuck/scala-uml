package pretty


import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import pretty.config.PrettyConfig
import uml.{AccessModifier, Note, Relationship, TopLevelElement, UMLElement, externalReferences}
import scalameta.util.namespaces.Entry
import pretty.Pretty._

trait PrettyPrinter[T <: UMLElement] {

  implicit val config:PrettyConfig

  def toDoc(umlElement: T):Doc

  def format(umlElement: T) : Document = {
    pretty(toDoc(umlElement))
  }

  protected def showNamespace(e:scalameta.util.namespaces.Entry) : Doc
  protected def showAccessModifier(accessModifier: AccessModifier):Doc

  protected def opt[T](opt:Option[T],
              show:T => Doc,
              l:Doc=emptyDoc,
              r:Doc=space,
              emptyL:Doc = emptyDoc,
              emptyR:Doc = emptyDoc)
  :Doc =
    opt.map(t => l <> show(t) <> r).getOrElse(emptyL <> emptyDoc <> emptyR)


}
