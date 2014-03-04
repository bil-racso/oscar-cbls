package oscar.dfo.utils

import oscar.util.RandomGenerator

object Utils {
  
  /** Returns a random normalized vector of the size specified as argument */
  def randomNormalizedVector(nDimensions: Int): Array[Double] = {
    normalizeArray(Array.tabulate(nDimensions)(i => if (RandomGenerator.nextInt(2) % 2 == 0) RandomGenerator.nextDouble else -1.0 * RandomGenerator.nextDouble))
  } 
  
  /** Returns the normalized version of the array passed as argument */
  def normalizeArray(coordinates: Array[Double]): Array[Double] = {
    val vectorLength = math.sqrt(coordinates.foldLeft(0.0)((acc, newDim) => acc + newDim * newDim))
    Array.tabulate(coordinates.length)(i => coordinates(i) / vectorLength)
  }
}