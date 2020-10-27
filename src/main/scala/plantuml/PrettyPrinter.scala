package plantuml

import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

/**
 * @todo Remove empty string from relationship
 * @todo cleanup opt mess
 */
object PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

  def format[T<:UMLElement](t: T) : Document = {
    super.pretty(show(t))
  }

  def show(umlElement: UMLElement): Doc = umlElement match {

    case UMLUnit(identifier, toplevelElements) =>
      "@startuml" <+> stringWrap(identifier) <@> vsep(toplevelElements.map(show)) <@> "@enduml"

    case Package(identifier, color, packageBodyElements, packageStyle) =>
      "package" <+> stringWrap(identifier) <+> noSpaceIfEmpty(showPackageStyle(packageStyle),
        optAndFurther(color,text,space,enclose("{",nest(line <> vsep(packageBodyElements.map(show))),line <> "}")))

    case GenericParameter(identifier, nested, lowerBound, upperBound) =>
      '<' <> identifier <>
        optWithSpace(emptyDoc,nested,show,
          optWithSpace(space <> "<:", lowerBound, text,
            optWithSpace( space <> ":>", upperBound, text,emptyDoc))) <> ">"

    case c@Class(isAbstract, identifier: String, classBodyElements, genericParameter, symbolDepiction) =>
      (if(isAbstract) {"abstract" <> space } else { emptyDoc }) <>
      "class" <+> stringWrap(identifier) <>
        opt(genericParameter,show) <> (if (c.stereotype.isDefined) {
          space <> "<<" <+>
            symbolDepiction.map(t => '(' <> text(t._1) <> ',' <> text(t._2) <> ')' <> space).getOrElse(emptyDoc) <>
              c.stereotype.get <+> ">>"
        } else {
          if(symbolDepiction.isDefined){
            space <> "<<" <+> '(' <> symbolDepiction.get._1 <> ',' <> symbolDepiction.get._2 <> ')' <+> ">>"
          } else {
            emptyDoc
          }
        }) <+> enclose("{",
        nest(line <> vsep(classBodyElements.map(show))),
        line <> "}")

    case a@Attribute(modificators, accessModifier, identifier, attributeType) =>
      opt(accessModifier,showAccessModifier,r=emptyDoc) <>
        showStereotype(a) <>
        opt(modificators,showModificators) <>
        identifier <+> ':' <+> attributeType

    case p@Parameter(identifier, paramType) =>
      showStereotype(p) <>
        identifier <+>
        ':' <+>
        paramType

    case o@Operation(modificators, accessModifier, identifier, paramSeq, returnType) =>
      showStereotype(o) <>
        opt(modificators,showModificators) <>
        opt(accessModifier,showAccessModifier) <>
        identifier <>
        hsep(paramSeq.map(params => '(' <> hsep(params.map(show),", ") <> ')')) <+>
        opt(returnType,text,":" <> space,r = emptyDoc)

    case Compartment(isHeading, lineType, identifier, compartmentElements) =>
      if(isHeading){
        showLineType(lineType) <> opt(identifier,text,l=space,r=space <> showLineType(lineType) ) <> line
      } else {emptyDoc} <>
      vsep(compartmentElements.map(show)) <>
        (if(!isHeading){
        showLineType(lineType) <> opt(identifier,text,l=space,r=space <> showLineType(lineType) ) <> line
      } else {emptyDoc})

    case c@CompartedClass(isAbstract,identifier,genericParameter,symbolDepiction,compartments) =>
      (if(isAbstract) {"abstract" <> space } else { emptyDoc}) <>
      "class" <+> stringWrap(identifier) <>
        opt(genericParameter,show) <>
        (if(c.stereotype.isDefined) {
        space <> "<<" <+>
          symbolDepiction.map(t => '(' <> text(t._1) <> ',' <> text(t._2) <> ')' <> space).getOrElse(emptyDoc) <>
            stringWrap(c.stereotype.get)

      } else {
        if(symbolDepiction.isDefined){
          space <> "<<" <+> '(' <> symbolDepiction.get._1 <> ',' <> symbolDepiction.get._2 <> ')' <+> ">>"
        } else {
          emptyDoc
        }
      } ) <+> enclose("{", vsep(compartments.map(show)), line <> "}")
    //@todo implement stereotype
    case DirectionNote(position,of,t) =>
      text("note") <+> showPosition(position) <+> "of" <+> stringWrap(of) <+> ':' <+> t

    case AliasNote(alias,t) =>
      text("note") <+> surround(t,'"') <+> "as" <+> alias

    case LinkNote(position,t) =>
      text("note") <+> showPosition(position) <+> "on link :" <+> t

    case SkinParam(args) =>
      "skinparam" <+> hsep(args.map(text))

    case Hide(args) =>
      "hide" <+> hsep(args.map(text))

    case r@Relationship(relationshipType,relationshipDirection,
      RelationshipInfo(sourceMultiplicity, targetMultiplicity, fromIdentifier,
      toIdentifier, relationshipIdentifier, identifierDirection)) =>
      stringWrap(fromIdentifier) <+>
      opt(sourceMultiplicity, (s:String) => surround(text(s),'"')) <>
      showRelationshipConnector(relationshipType,relationshipDirection) <+>
      opt(targetMultiplicity, (s:String) => surround(text(s),'"')) <>
      stringWrap(toIdentifier) <+>
        (if(relationshipIdentifier.isDefined || r.stereotype.isDefined) {
        ":" <+>
          (if (identifierDirection.equals(ToFrom)) {
            "<" <> space
          } else {
            emptyDoc
          }) <>
          (if (r.stereotype.isDefined) {
            '"'
          } else {
            emptyDoc
          }) <> showStereotype(r) <>
          opt(relationshipIdentifier, text) <>
          (if (r.stereotype.isDefined) {
            '"'
          } else {
            emptyDoc
          }) <>
          (if (identifierDirection.equals(FromTo)) {
            space <> ">" <> space
          } else {
            emptyDoc
          })
      } else {
        emptyDoc
      })
  }

  /**
   * @todo replace occurence with opt
   * @param l
   * @param opt
   * @param show
   * @param r
   * @param default
   * @tparam T
   * @return
   */
  def optWithSpace[T](l:Doc,opt:Option[T],show:T => Doc,r:Doc,default:Doc=emptyDoc) : Doc =
    if(opt.isDefined){
       l <+> show(opt.get) <+> r
    }else {
      default
    }

  def opt[T](opt:Option[T],show:T => Doc,l:Doc=emptyDoc,r:Doc=space,emptyR:Doc = emptyDoc):Doc =
    opt.map(t => l <> show(t) <> r).getOrElse(emptyDoc<>emptyR)
  /**
   * Creates a `Doc` that contains the `further` document correctly separated with spaces with respect to the optional
   * parameter `toShow`.
   *
   * @param toShow included in the Document if present
   * @param show method to convert `toShow` into a document
   * @param further document that is shown after the optional document `toShow`
   * @tparam T Type of the element that is optionally shown
   * @return Document correctly separated with spaces with respect to `toShow`
   */
  def optAndFurther[T](toShow:Option[T],show:T => Doc, separation:Doc, further:Doc) : Doc =
    if (toShow.isDefined) toShow.map(show).get <> separation <> further else further

  /**
   * @param s possibly empty string
   * @param further Anything that follows `s`.
   * @return `Doc` that contains `s` if `s` is not empty.
   */
  def noSpaceIfEmpty(s:String,further:Doc):Doc = if (s.isEmpty) further else s <+> further

  /**
   * Encloses a string with string literals if the string is more complex than a simple word.
   *
   * PlantUML allows identifier of objects to contain spaces and newline characters, if so the
   * identifier has to be wrapped as a string.
   *
   * @param text text to be wrapped
   * @return `text` wrapped in string literals if `text` contains spaces
   */
  def stringWrap(text:String):Doc = if (text.contains(" ")) surround(text,'"') else text

  def showStereotype(stereotypeElement: StereotypeElement):Doc =
    opt(stereotypeElement.stereotype,text,"<<" <> space,space <> ">>")

  def showPackageStyle(packageStyle: PackageStyle):String = packageStyle match {
    case Default => ""
    case Node =>"<<Node>>"
    case Rectangle =>"<<Rectangle>>"
    case Folder =>"<<Folder>>"
    case Frame =>"<<Frame>>"
    case Cloud =>"<<Cloud>>"
    case Database =>"<<Database>>"
  }

  def showAccessModifier(accessModifier: AccessModifier):String = accessModifier match {
    case Private => "-"
    case Protected => "#"
    case PackagePrivate => "~"
    case Public => "+"
  }

  def showModificators(modificators: List[Modificator]):String = modificators match {
    case Static :: mods => " {static}"
    case Abstract :: mods => " {abstract}"
    case Nil => ""
  }

  def showLineType(lineType: LineType):String = lineType match {
    case Single => "--"
    case Dotted =>".."
    case Double => "=="
    case ThickSingle => "__"
  }

  def showPosition(position: Position):String = position match {
    case Left => "left"
    case Right => "right"
    case Top => "top"
    case Bottom => "bottom"
  }

  def showIdentifierDirection(relationshipDirection: RelationshipDirection) : String = relationshipDirection match {
    case FromTo => ">"
    case ToFrom => "<"
    case Without => ""
  }

  def showRelationshipConnector(relationshipType: RelationshipType,relationshipDirection: RelationshipDirection):String = relationshipType match {
    case Extension => appendRelationshipEnd("<|","--",relationshipDirection)
    case Composition => appendRelationshipEnd("*","--",relationshipDirection)
    case Aggregation => appendRelationshipEnd("o","--",relationshipDirection)
    case Annotation => appendRelationshipEnd("<","..",relationshipDirection)
    case Association => appendRelationshipEnd("<","--",relationshipDirection)
    case Inner => appendRelationshipEnd("+","--",relationshipDirection)
    case Note => appendRelationshipEnd("","..",relationshipDirection)
  }

  def appendRelationshipEnd(relEnd:String,relationship:String,relationshipDirection: RelationshipDirection):String = relationshipDirection match {
    case FromTo => relationship + inverse(relEnd)
    case ToFrom => relEnd + relationship
    case Without => relationship
  }

  def inverse(s:String) = s match {
    case "<|" => "|>"
    case "<" => ">"
    case "*" => "*"
    case "o" => "o"
    case "+" => "+"
  }

}
