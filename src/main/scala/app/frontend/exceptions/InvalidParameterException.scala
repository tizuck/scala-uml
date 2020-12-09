package app.frontend.exceptions

import org.bitbucket.inkytonik.kiama.parsing.NoSuccess

class InvalidParameterException(message:String, noSucc:NoSuccess) extends Exception(message) {

  def this(message:String,cause:Throwable,noSucc:NoSuccess) {
    this(message,noSucc)
    initCause(cause)
  }

  def this(cause:Throwable,noSucc:NoSuccess){
    this(Option(cause).map(_.toString).orNull,cause,noSucc)
  }

  def this(noSucc:NoSuccess) {
    this(null: String,noSucc)
  }

  def getNoSucc():NoSuccess = noSucc

}