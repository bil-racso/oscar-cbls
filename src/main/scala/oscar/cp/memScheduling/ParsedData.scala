package oscar.cp.memScheduling

class ParsedData(var data: Array[Array[String]]) {
  
  def string: 			Array[Array[String]]		= data
	def int: 					Array[Array[Int]]				= data.map(_.map(_.toInt))
  def double: 			Array[Array[Double]] 		= data.map(_.map(_.toDouble))
  def simpleValue: 	Array[String]						=	data.slice(1, data.length).map(_(0))
  
  def extract(nbElements: Int): ParsedData 	= ParsedData(splitAndUpdateAt(nbElements))
  def extractMatrixOf(nbCol: Int) = ParsedData(splitAndUpdateAt(nbCol).transpose)
  
  private def splitAndUpdateAt(n: Int): Array[Array[String]] = {
    val (extractedData, reducedData) = data.splitAt(n)
    data = reducedData
    return extractedData
  }
  
}

object ParsedData {
  def apply(data: Array[Array[String]]): ParsedData = {
    new ParsedData(data)
  }
}