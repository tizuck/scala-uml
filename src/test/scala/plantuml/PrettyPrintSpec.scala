package plantuml

import org.scalatest.flatspec.AnyFlatSpec

class PrettyPrintSpec extends AnyFlatSpec {
  "A Plant UML File with two classes" should "be printed correctly" in {
    println(PlantUMLUnit("Two classes",Seq(
      Class(false,"Class1",Seq.empty,None,None)(Some("case class")),
      Class(false,"BaseTrait",
        Seq(
          Attribute(None,None,"s","String")(None),
          Operation(None,None,"foo",
            Seq(Seq(Parameter("s","String")(None))),
            "String"
          )(None)
        ).asInstanceOf[Seq[ClassBodyElement]],None,None)(Some("trait")),
      Relationship(Extension,FromTo,RelationshipInfo(None,None,"Class1","BaseTrait",None,Without))(stereotypeN = None),
      SkinParam(Seq("ClassBackgroundcolor","White")),
      SkinParam(Seq("ClassArrowColor","Black")),
      SkinParam(Seq("ClassBorderColor","Black")),
      Hide(Seq("class","circle")),
      Hide(Seq("class","members"))
    ).asInstanceOf[Seq[TopLevelElement]]).pretty)
  }
}
