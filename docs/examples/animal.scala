package animals

trait Animal {
  val location : String
}

sealed case class Wombat(
                          location:String
                        ) extends Animal
