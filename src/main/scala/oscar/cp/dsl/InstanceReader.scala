package oscar.cp.dsl

import scala.io.Source
import scala.collection.mutable.ListBuffer

class InstanceReader(filepath: String) {
  val file = Source.fromFile(filepath).getLines
  val splitRegexp = " +"
  val allRemaining = Int.MaxValue
  
  implicit def parsedNumericalStringArray(s: Array[String]) = new {
    def asInt = s.map(_.toInt)
    def asDouble = s.map(_.toDouble)
  }
  
  implicit def parsedNumericalFile(s: Array[Array[String]]) = new {
    def asInt = s.map(_.map(_.toInt))
    def asDouble = s.map(_.map(_.toDouble))
  }
  
  def nextLine = file.next.trim
  
  /**
   * Return the next line of the file split on spaces as an array of String.
   */
  def readLine: Array[String] = nextLine.split(splitRegexp)
  
  /**
   * Reads lines of the file by batches and builds an array of nbColumns+1 arrays containing
   * the nbColumns values describing each of the nbElement elements, prepended of the array
   * of elements indices. If the end of file is reached, the resulting arrays may not contain
   * nbElements elements. This is obviously the case when using the value allRemaining.
   * For each element, it will read lines until it has read nbColumns values.
   * The first array contains the batch numbers from which the other datas have been read.
   * The following arrays contains the datas read from the nbColumns columns.
   * All returned values are String so they can be converted to the right type afterwards.
   * 
   * Example:
   * --------
   * Calling readDatas(2, 5) on the following file
   * 2 4 6
   * 7 1
   * 8 0 2
   * 9 3
   * 0 9 1
   * 4 2
   * 
   * Will try to read 2 elements described by 5 columns and will lead to the following array:
   * [[0 1]		// Batches index other datas are coming from. In this example we read 2 batches of 2 lines.
   *  [2 8]		// Values found in the first column of the three batches of lines we read. 
   *  [4 0]		// Values found in the second column of the three batches of lines we read.
   *  [6 2]		// Values found in the third column of the three batches of lines we read.
   *  [7 9]		// Values found in the fourth column of the three batches of lines we read.
   *  [1 3]]	// Values found in the fifth column of the three batches of lines we read.
   */
  def readDatas(nbElement: Int, nbColumns: Int): Array[Array[String]] = {
    val datas = Array.fill(nbColumns+1){new ListBuffer[String]}
    var elementIndex = 0
    while(elementIndex < nbElement && file.hasNext) {
      // Read lines by batches
      val readValues = new ListBuffer[String]
	    while (readValues.length < nbColumns && file.hasNext) {
	      val line = nextLine
	      // TODO Test if line is comment or something else, if it is actually valid!
	      if (! "".equals(line)) readValues ++= line.split(splitRegexp)
	    }
	    
	    // Classify values into their respectful arrays
	    for (values <- readValues.grouped(nbColumns)){
	      datas(0) append elementIndex.toString
	    	for (dataIndex <- 0 until nbColumns){
	    		datas(dataIndex+1) += values(dataIndex)
	    	}
	    }
	    elementIndex += 1
	  }
    return datas.map(_.toArray)
  }

}