package oscar.cp.memScheduling

class ParsedLine(line: Array[String]) {
  
  	def string: Array[String]	= line
    def int: 		Array[Int]		= line.map(_.toInt)
    def double: Array[Double] = line.map(_.toDouble)
    
    def asIntArrayFillerOf(nbElements: Int): Array[Int] = {
      val baseIntArray = line.map(_.toInt)
      Array.tabulate(nbElements)(index => baseIntArray(index % line.length))
    }
  	
    def asDoubleArrayFillerOf(nbElements: Int): Array[Double] = {
      val baseIntArray = line.map(_.toDouble)
      Array.tabulate(nbElements)(index => baseIntArray(index % line.length))
    }
 
}

object ParsedLine {
  def apply(data: Array[String]): ParsedLine = {
    new ParsedLine(data)
  }
}