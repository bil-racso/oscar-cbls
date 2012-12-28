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
  
  def parseMultiSet(filepath: String, n: Int): Array[Array[(Int, Int)]] = { 
    
    var lines = Source.fromFile(filepath).getLines.toList
    
    val sets = Array.fill(n)(List[(Int, Int)]())
    
    var i = 0
    lines = lines.drop(1)
    
    for (j <- 0 until lines.size) {
      val line = lines.head
    
      if (lines.head.head == 'L')
        i += 1
      else {
        val l = lines.head.trim.split(" ").map(_.toInt).toArray
        val x = l(0)
        val y = l(1)
        sets(i) = (x, y) :: sets(i)
      }
      
      lines = lines.drop(1)
    }
    
    sets.map(_.toArray)
  }
}