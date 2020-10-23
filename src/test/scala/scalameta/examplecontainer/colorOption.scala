package scalameta.examplecontainer

object colorOption {
  val program : String =
    """object Main {
      |
      |  enum Color(val rgb: Int) {
      |    case Red   extends Color(0xFF0000)
      |    case Green extends Color(0x00FF00)
      |    case Blue  extends Color(0x0000FF)
      |  }
      |
      |  trait Ord[T] {
      |    def compare(x:T,y:T):Int
      |    extension (x: T) def < (y: T) = compare(x,y) < 0
      |    extension (x: T) def > (y:T) = compare(x,y) > 0
      |  }
      |
      |  //Red > Green > Blue
      |  given colorOrd as Ord[Color] {
      |    def compare(x : Color,y : Color) : Int = x match {
      |      case Color.Red => 1
      |      case Color.Green => y match {
      |        case Color.Red => -1
      |        case Color.Green => 0
      |        case _ => 1
      |      }
      |      case Color.Blue => y match {
      |        case Color.Blue => 0
      |        case _ => -1
      |      }
      |    }
      |  }
      |
      |  enum Option[+T] {
      |    case Some(x: T)
      |    case None
      |
      |    def isDefined: Boolean = this match {
      |      case None => false
      |      case some => true
      |    }
      |  }
      |
      |  def main(args: Array[String]): Unit = {
      |    println("Hello world!")
      |  }
      |}
      |""".stripMargin

}
