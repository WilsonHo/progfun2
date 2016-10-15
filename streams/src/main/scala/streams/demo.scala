package streams

import Bloxorz._
/**
  * Created by baohg on 06/10/2016.
  */
object demo extends GameDef with Solver with StringParserTerrain {
  def main(args: Array[String]): Unit = {
    val a = Pos(1, 1)
    val b = Block(a, a)
    println(b)
    b.b1.dx(2)
    b.b2.dx(4)
    println(b)

  }

  override val level: String = ""
}
