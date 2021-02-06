package pretty


import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import pretty.config.PrettyConfig
import pretty.KiamaPretty._
import uml.{AccessModifier, UMLElement}

trait PrettyPrinter[T <: UMLElement] {

  implicit val config:PrettyConfig

  def toDoc(umlElement: T):Doc

  def format(umlElement: T) : Document = {
    pretty(toDoc(umlElement))
  }

  protected def showNamespace(e:scalameta.util.namespaces.Entry) : Doc
  protected def showAccessModifier(accessModifier: AccessModifier):Doc

  protected def opt[InnerT](opt:Option[InnerT],
                            show:InnerT => Doc,
                            l:Doc=emptyDoc,
                            r:Doc=space,
                            emptyL:Doc = emptyDoc,
                            emptyR:Doc = emptyDoc)
  :Doc =
    opt.map(t => l <> show(t) <> r).getOrElse(emptyL <> emptyDoc <> emptyR)
}
