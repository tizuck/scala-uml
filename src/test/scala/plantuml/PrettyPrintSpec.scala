package plantuml

import org.scalatest.flatspec.AnyFlatSpec

class PrettyPrintSpec extends AnyFlatSpec {
  "A Plant UML File with two classes" should "be printed correctly" in {
    println(PlantUMLUnit("Two classes",List(
      Class(false,"Class1",List.empty,None,None)(Some("case class")),
      Class(false,"BaseTrait",
        List(
          Attribute(None,None,"s","String")(None),
          Operation(None,None,"foo",
            List(List(Parameter("s","String")(None))),
            Some("String")
          )(None)
        ).asInstanceOf[List[ClassBodyElement]],None,None)(Some("trait")),
      Relationship(Extension,FromTo,RelationshipInfo(None,None,"Class1","BaseTrait",None,Without))(stereotypeN = None),
      SkinParam(List("ClassBackgroundcolor","White")),
      SkinParam(List("ClassArrowColor","Black")),
      SkinParam(List("ClassBorderColor","Black")),
      Hide(List("class","circle")),
      Hide(List("class","members"))
    ).asInstanceOf[List[TopLevelElement]]).pretty)
  }
}
