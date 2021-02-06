package app.frontend.exceptions

class NoParametersProvidedException(message:String) extends Exception(message) {

  def this(message:String,cause:Throwable) {
    this(message)
    initCause(cause)
  }

  def this(cause:Throwable){
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }

}
