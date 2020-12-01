package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{Note, PackageBodyElement, Relationship}

case class PackageBodyPretty()(override implicit val config: PrettyConfig)
  extends PlantUMLPrettyPrinter[PackageBodyElement] {

  override def toDoc(umlElement: PackageBodyElement): KiamaPretty.Doc = umlElement match {
    case p:uml.Package  => PackagePretty().toDoc(p)
    case c:uml.Class    => ClassPretty().toDoc(c)
    case n:Note         => NotePretty().toDoc(n)
    case r:Relationship => RelationshipPretty().toDoc(r)
  }
}
