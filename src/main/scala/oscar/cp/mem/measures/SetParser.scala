package oscar.cp.mem.measures

import scala.io.Source

object SetParser {

  def parseSet(filepath: String): Array[(Int, Int)] = { 
    var lines = Source.fromFile(filepath).getLines.toList
    val nLines = lines.size
    val set = Array.tabulate(nLines)(i => {
      val l = lines.head.trim.split(" ").map(_.toInt).toArray
      val x = l(0)
      val y = l(1)
      lines = lines.drop(1)
      (x, y)
    })
    set
  }
}