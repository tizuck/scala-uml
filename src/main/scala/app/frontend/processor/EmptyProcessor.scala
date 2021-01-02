package app.frontend.processor

case object EmptyProcessor extends Processor {
  override def execute(): Unit = {
    UMLDiagramProcessor("","",false,false).execute()
  }
}
