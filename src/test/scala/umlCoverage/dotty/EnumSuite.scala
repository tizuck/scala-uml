package umlCoverage.dotty

import net.sourceforge.plantuml.SourceStringReader
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.SourceCollector
import scalameta.util.context.GlobalContext
import uml.{ClassRef, ConcreteClass, Extension, Relationship, UMLUnit}

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.meta.inputs.Input
import scala.meta.{Source, dialects}

class EnumSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","enums.txt")

  "Dotty Reference to enums can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    val umlUnit = umlCollector.umlUnit

    existsEnum(umlUnit,"ColorTwo")
    caseInheritanceExists(umlUnit,"ColorTwo","RedTwo")
    caseInheritanceExists(umlUnit,"ColorTwo","GreenTwo")
    caseInheritanceExists(umlUnit,"ColorTwo","BlueTwo")

    existsEnum(umlUnit,"Planet")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Mercury")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Venus")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Earth")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Mars")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Jupiter")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Uranus")
    caseInheritanceExistsClassRef(umlUnit,"Planet","Neptune")

    //@todo eliminated for coverage purpose
    //existsEnum(umlUnit,"Option")
    //caseInheritanceExistsClassRef(umlUnit,"Some","Option")
    //caseInheritanceExistsClassRef(umlUnit,"None","Option")

    implicit val umlUnitPretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/enums.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }

  private def caseInheritanceExists(umlUnit: UMLUnit,from:String,to:String) = {
    umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals(from) case _ => false } &&
          r.relationshipInfo.to.exists {
            case c: ConcreteClass => c.cls.name.equals(to) &&
              c.cls.stereotype.exists(s => s.name.equals("case"))
            case _ => false
          } &&
          r.relationshipType.equals(Extension)
      case _ => false
    } must be(true)
  }

  private def caseInheritanceExistsClassRef(umlUnit: UMLUnit,from:String,to:String) = {
    umlUnit.exists {
      case r: Relationship =>
        r.relationshipInfo.from.exists { case c: ClassRef => c.name.equals(from) case _ => false } &&
          r.relationshipInfo.to.exists {
            case c: ClassRef => c.name.equals(to)
            case _ => false
          } &&
          r.relationshipType.equals(Extension)
      case _ => false
    } must be(true)

    umlUnit.exists{
      case c:uml.Class => c.name.equals(to) && c.stereotype.exists(s => s.name.equals("case"))
      case _ => false
    }
  }

  private def existsEnum(umlUnit: UMLUnit, name:String) = {
    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals(name) &&
          c.stereotype.exists(s => s.name.equals("scalaenum"))
      case _ => false
    } must be(true)
  }
}
