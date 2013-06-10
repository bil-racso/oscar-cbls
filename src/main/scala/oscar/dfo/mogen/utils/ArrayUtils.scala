package oscar.dfo.mogen.utils

object ArrayUtils {
  /** Returns a new array that is the sum index by index of input arrays. */
  def arraySum(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) + ar2(i))
  
  /** Returns a new array that is the difference index by index of input arrays. */
  def arrayDiff(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) - ar2(i))
  
  /** Returns a new array that is the scalar product of input the input factor and the array. */
  def arrayProd(ar: Array[Double], factor: Double): Array[Double] = Array.tabulate(ar.length)(i => factor * ar(i))
  
  /** Returns the euclidian distance between the two input arrays. */
  def euclidianDistance(coord1: Array[Double], coord2: Array[Double]): Double = {
    math.sqrt(coord1.zip(coord2).foldLeft(0.0)((acc, newPair) => acc + math.pow(newPair._2 - newPair._1, 2.0)))
  }

  /** Returns the array normalized. */
  def normalize(ar: Array[Double]): Array[Double] = {
    val arrayLength = math.sqrt(ar.foldLeft(0.0)((acc, coord) => acc + math.pow(coord, 2)))
    Array.tabulate(ar.length)(i => ar(i) / arrayLength)
  }  
}