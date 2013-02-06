package oscar.dfo.algo

trait ComparativeAlgorithmState {
  /** A function allowing to check if a point is in the feasible region */
  val feasibleRegion: (Array[Double] => Boolean)
}