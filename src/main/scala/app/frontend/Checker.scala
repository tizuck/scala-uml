package app.frontend

import cats.data.State

case object Checker {

  def checkCommandList(commands:List[Command]) = {
    helpSoleProp(commands)
    uniqueProp(commands)
  }

  /**
   * Each command should only occur once.
   *
   * @param commands user provided commands.
   */
  private def uniqueProp(commands:List[Command]) : Unit = {
      val res = commands
        .forall{
          c => commands.count{
            cOther => c.isInstanceOf[cOther.type]
          } == 1
        }
      if(!res){
        throw new IllegalArgumentException("Don't use multiple ")
      }
  }

  /**
   * Help cannot be used together with other commands.
   *
   * @param commands user provided commands
   */
  private def helpSoleProp(commands:List[Command]) : Unit = {
        if(!commands
          .forall({case _:Help => commands.size == 1 case _ => true})){
          throw new IllegalArgumentException(" " +
            "--help cannot be used together with other commands. try: --help as sole command for help or\n" +
            " --help <command> for help to specific command.")
        }
  }
}
