package app.frontend.processor

import uml.UMLUnit

case object EmptyProcessor extends Processor {
  override def execute(): UMLUnit = {
    UMLDiagramProcessor().execute()
    UMLUnit("",Nil)
  }
}
