package oscar.dfo.algo

trait ComparativeAlgorithm {
  
  /** Solves an optimization problem defined by its starting point, its comparison function and its feasible region
    * 
    * @param startPoint
    */
  def solve(startPoint: Array[Double], comparison: (Array[Double], Array[Double]) => Array[Double], feasibleRegion: Array[Double] => Boolean): Array[Double]

}