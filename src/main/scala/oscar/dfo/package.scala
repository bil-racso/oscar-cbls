package oscar

package object dfo {
  
  val rand = new scala.util.Random(12)
  
  object DFOAlgo extends Enumeration {
    val NelderMead = Value("NelderMead")
    val DDS = Value("DDS")
    val MDS = Value("MDS")
  }

}