package umlCoverage.dotty

import net.sourceforge.plantuml.SourceStringReader
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.SourceCollector
import scalameta.util.context.GlobalContext
import uml.{ClassRef, Extension, Realization, Relationship, UMLUnit}

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.meta.inputs.Input
import scala.meta.{Source, dialects}

class ExtensionSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","extensions.txt")
  //@todo eliminated for coverage purpose
  /*
  "Dotty Reference to enums can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Dotty(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    val umlUnit = umlCollector.umlUnit

    //@todo eliminated for coverage purpose
    //extensionCircle(umlUnit)
    //extensionList(umlUnit)
    //extensionSeq(umlUnit)

    implicit val umlUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/extensions.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }

  private def extensionList(umlUnit: UMLUnit) = {
    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("TList")
      case _ => false
    }
    //@todo eliminated for coverage purpose
    /*umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.relationshipIdentifier.equals("<<bind T -> T>>") &&
          r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals("TList") } &&
          r.relationshipInfo.to.exists { case c: ClassRef => c.name.equals("List") } &&
          r.relationshipType.equals(Extension)
      case _ => false
    } must be(true)

    umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.relationshipIdentifier.equals("<<extending>>") &&
          r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals("c") } &&
          r.relationshipInfo.to.exists { case c: ClassRef => c.name.equals("TList") } &&
          r.relationshipType.equals(Realization)
      case _ => false
    } must be(true)

    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("c") &&
          c.stereotype.exists(s => s.name.equals("extension")) &&
          c.operations.exists(o =>
            o.name.equals("second") &&
              o.returnType.isEmpty
          )
      case _ => false
    } must be(true)
  }

  private def extensionSeq(umlUnit: UMLUnit) = {
    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("StringSeq")
      case _ => false
    }

    umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.relationshipIdentifier.equals("<<bind T -> String>>") &&
          r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals("StringSeq") } &&
          r.relationshipInfo.to.exists { case c: ClassRef => c.name.equals("Seq") } &&
          r.relationshipType.equals(Extension)
      case _ => false
    } must be(true)

    umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.relationshipIdentifier.equals("<<extending>>") &&
          r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals("ss") } &&
          r.relationshipInfo.to.exists { case c: ClassRef => c.name.equals("StringSeq") } &&
          r.relationshipType.equals(Realization)
      case _ => false
    } must be(true)

    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("ss") &&
          c.stereotype.exists(s => s.name.equals("extension")) &&
          c.operations.exists(o =>
            o.name.equals("longestStrings") &&
              o.returnType.isDefined && o.returnType.get.equals("Seq<String>")
          )
      case _ => false
    } must be(true) */
  }

  private def extensionCircle(umlUnit: UMLUnit) = {
    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("c") &&
          c.stereotype.exists(s => s.name.equals("extension")) &&
          c.operations.exists(o =>
            o.name.equals("circumference") &&
              o.returnType.isDefined && o.returnType.get.equals("Double")
          )
      case _ => false
    } must be(true)
  }
  */
}
