/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.dfo.algo

import oscar.dfo.utils._

/** Framework for the Multi-Directional Search (MDS) algorithm used to find the optimum
  * of a derivative free optimization (DFO) problem.
  *
  * @constructor Create a new MDS framework for the problem specified by its function, the
  *              comparison function for evaluations and the domain around the specified
  *              starting point
  * @param f Function to optimize
  * @param nbObjectives The number of objectives to optimize
  * @param compare The comparison function to compare evaluations
  * @param startP The starting point from which the algorithm will begin
  * @param dom The domain of the search space
  * @throws IllegalArgumentException If the starting point doesn't lie within the
  *                                     domain or if the starting point and the domain don't have
  *                                     the same dimensions 
  * @author Cyrille Dejemeppe 
  */
class MDS(f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean, startP: Array[Double], dom: Array[Interval]) extends DFOptimizer {
  
  /** The dimension of the search space */
  val dim = startP.length
  
  /** The intervals defining the domain of the function */
  val domains = Array.tabulate(dim)(i => dom(i))
  var startPoint = startP
  
  /** The max used for the confidence interval size */
  var max = dom(0).size
  for (i <- 1 until dom.length) {
    if (dom(i).size < max)
      max = dom(i).size
  }
  max /= 5.0
  
  //Basic check #1
  for(i <- 0 until dim)
  {
    if(!(domains(i).isInInterval(startPoint(i))))
      throw new IllegalArgumentException("The starting point must be in the function domain.")
  }
  
  //Basic check #2
  if(startPoint.length != dom.length)
    throw new IllegalArgumentException("The starting point and the domain intervals must be of the same dimension.")
  
  /** The expansion coefficient */
  val gammaE = 5.0
  /** The shrinking coefficient */
  val gammaS = 0.3
  
  /** The number of iterations performed by the algorithm */
  var iterCount = 0
  evalCount = dim + 1
  /** The number of rotations applied */
  var rotCount = 0
  /** The number of expansions applied */
  var expCount = 0
  /** The number of shrinks applied */
  var shrCount = 0
  
  // Initialization of the simplex data structure.
  simplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  
  var currentBest:(Array[Double], Array[Double]) = simplex(0)

  /** Fills the simplex data structure with an initial configuration.
    * 
    * Fills the simplex data structure with relevant points: the starting point and
    * other points defined as p_i = the starting point + Width(domain(dim_i)) / 20 
    */
  def buildSimplex = {
    simplex(0) = (startPoint, f(startPoint))
    for (i <- 1 to dim) {
      val tmp = putInDomain(Array.tabulate(dim)(j => if (i == j + 1) startPoint(j) + dom(j).size/20.0 else startPoint(j)))
      simplex(i) = (tmp, f(tmp))
    }
  }
  
  /** Simplex structure of the previous iteration */
  var formerSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  for (i <- 0 to dim) {
    formerSimplex(i) = (Array.ofDim[Double](dim), Array.fill(nbObjectives){Double.MaxValue})
  }
  
  /** Puts all the class variables to their initial values */
  def reset = {
    iterCount = 0
    evalCount = 0
    rotCount = 0
    expCount = 0
    shrCount = 0
    buildSimplex
    copySimplex
  }
  
  reset
  
  //Sort the array such that the points are sorted in the increasing order wrt their function evaluation.
  scala.util.Sorting.stableSort(simplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue)
  
  /** Returns the comparison of the former and the new simplices.
    * 
    * @return True if formerSimplex and simplex contains the same points
    */
  def compareSimplex: Boolean = {
    for (i <- 0 to dim) {
      if (!(formerSimplex(i).equals(simplex(i))))
        return false
    }
    true
  }
  
  /** Copies simplex to formerSimplex */
  def copySimplex = {
    for (i <- 0 to dim) {
      formerSimplex(i) = (simplex(i)._1, simplex(i)._2)
    }
  }
  
  def putInDomain(x: Array[Double]): Array[Double] = {
    Array.tabulate(dim)(i => if (domains(i).isInInterval(x(i))) x(i) else domains(i).getClosestBound(x(i)))
  }
  
  /** Returns a simplex that is simplex rotated around the best vertex.
    * 
    * Every point y_i in the rotated simplex is defined as follows:
    * y_i = y_0 - (y_i - y_0).
    * 
    * @return An array representing a simplex data structure, each vertex of the simplex
    *         being a pair (point, evaluations) that is the 180° rotation around the first
    *         vertex of simplex. This simplex data structure is sorted in increasing order
    *         according to the compare function.
    */
  def rotateSimplex(): Array[(Array[Double], Array[Double])] = {
    var rotSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
    rotSimplex(0) = (simplex(0)._1, simplex(0)._2)
    for (i <- 1 to dim) {
      val rPoint = putInDomain(Array.tabulate(dim)(j => simplex(0)._1(j) - (simplex(i)._1(j) - simplex(0)._1(j))))
      val eval = evaluator.eval(f, rPoint)
      evalCount += eval._2
      rotSimplex(i) = (rPoint, eval._1)
    }
    //Sort the array such that the points are sorted in the increasing order of their function evaluation.
    scala.util.Sorting.stableSort(rotSimplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue())
    rotSimplex
  }
  
  /** Returns a simplex that is simplex expanded around the best vertex.
    * 
    * Every point y_i in the expanded simplex is defined as follows:
    * y_i = y_0 - gammaE * (y_i - y_0).
    * @return An array representing a simplex data structure, each vertex of the simplex
    *         being a pair (point, evaluations) that is the expansion around the first
    *         vertex of simplex. This simplex data structure is sorted in increasing order
    *         according to the compare function.
    */
  def expandSimplex(): Array[(Array[Double], Array[Double])] = {
    var expSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
    expSimplex(0) = (simplex(0)._1, simplex(0)._2)
    for (i <- 1 to dim) {
      val ePoint = putInDomain(Array.tabulate(dim)(j => simplex(0)._1(j) - gammaE * (simplex(i)._1(j) - simplex(0)._1(j))))
      val eval = evaluator.eval(f, ePoint)
      evalCount += eval._2
      expSimplex(i) = (ePoint, eval._1)
    }
    //Sort the array such that the points are sorted in the increasing order of their function evaluation.
    scala.util.Sorting.stableSort(expSimplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue())
    expSimplex
  }
  
  /** Returns a simplex that is simplex shrunk around the best vertex.
    * 
    * Every point y_i in the rotated simplex is defined as follows:
    * y_i = y_0 + gammaS * (y_i - y_0).
    * 
    * @return An array representing a simplex data structure, each vertex of the simplex
    *         being a pair (point, evaluations) that is the shrinking around the first
    *         vertex of simplex. This simplex data structure is sorted in increasing order
    *         according to the compare function.
    */
  def shrinkSimplex(): Array[(Array[Double], Array[Double])] = {
    var shrSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
    shrSimplex(0) = (simplex(0)._1, simplex(0)._2)
    for (i <- 1 to dim) {
      val sPoint = putInDomain(Array.tabulate(dim)(j => simplex(0)._1(j) + gammaS * (simplex(i)._1(j) - simplex(0)._1(j))))
      val eval = evaluator.eval(f, sPoint)
      evalCount += eval._2
      shrSimplex(i) = (sPoint, eval._1)
    }
    //Sort the array such that the points are sorted in the increasing order of their function evaluation.
    scala.util.Sorting.stableSort(shrSimplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue())
    shrSimplex
  }
  
  override def simplexDiameter: Double = {
    var maxDist = Double.MinValue
    for (i <- 1 to dim) {
      val tmpDist = pointDistance(simplex(i)._1, simplex(0)._1)
      maxDist = if(tmpDist > maxDist) tmpDist else maxDist
    }
    maxDist
  }
  
  def iter(): List[(Array[Double], Array[Double])] = {
    val formerBest = simplex(0)
    var toRet = List[(Array[Double], Array[Double])]()
    iterMDS()
    var i = 0
    while (i < simplex.length && !areEqual(simplex(i)._2, formerBest._2)) {
      toRet = toRet ::: List(simplex(i))
      i += 1
    }
    toRet
  }
  
  /** Performs one iteration of the MDS algorithm and returns a Boolean indicating if the simplex need to be re-sorted.
    *
    * @return A Boolean that is True if the simplex need to be re-sorted, false otherwise */
  def iterMDS() = {
    //Compute the rotation of the simplex.
    val rotSimplex = rotateSimplex()
    //Expansion of the simplex attempted.
    if (compare(rotSimplex(0)._2, simplex(0)._2)) {
      val expSimplex = expandSimplex()
      //The expansion provides a better result than the rotation.
      if (compare(expSimplex(0)._2, rotSimplex(0)._2)) {
        expCount += 1
        for (i <- 0 to dim)
          simplex(i) = expSimplex(i)
      }
      //The rotation provides a better result than the expansion.
      else {
        rotCount += 1
        for (i <- 0 to dim)
          simplex(i) = rotSimplex(i)
      }
    }
    //Shrinking the simplex
    else {
      val shrSimplex = shrinkSimplex()
      shrCount += 1
      for (i <- 0 to dim)
        simplex(i) = shrSimplex(i)
    }
  }
  
  override def optimize(tol: Double, evalLimit: Int, timeLimit: Int): (Array[Double], Array[Double]) = {
    var diameter = Double.MaxValue
    var stop = false
    val begTime = System.currentTimeMillis()
    while (!stop) {
      //Checking if a stop criterion occurs.
      if (simplexDiameter < tol || evalCount >= evalLimit || (System.currentTimeMillis() - begTime)/1000 >= timeLimit)
        stop = true
      if (! stop) {
        iterMDS()
        iterCount +=1
        //Check if the last iteration brought some progress.
        if (compareSimplex)
          stop = true
        else {
          currentBest = simplex(0)
          onImprovement()
        }
        copySimplex
      }
    }
    simplex(0)
  }
  
  override def sampledOptimize(nbSamples: Int, tol: Double): (Array[Double], Array[Double]) = {
    var fctCallCount = 0
    val samplePoints = sampler.scrambledHaltonSequence(nbSamples, dom)
    var bestResult = (Array.fill(dom.length){0.0}, Array.fill(nbObjectives){Double.MaxValue})
    for (i <- 0 until nbSamples) {
      startPoint = samplePoints(i)
      reset
      val curResult = optimize(math.pow(10,-2), 10000, 100)
      fctCallCount += evalCount
      if(compare(curResult._2, bestResult._2))
        bestResult = (curResult._1, curResult._2)
    }
    evalCount = fctCallCount
    bestResult
  }
}

/** Factory for MDS instances. */
object MDS {
  /** Create a MDS according to the specified arguments.
    * 
    * @param f Function to optimize
    * @param nbObjectives The number of objectives to optimize
    * @param compare The comparison function to compare evaluations
    * @param startP The starting point from which the algorithm will begin
    * @param dom The domain of the search space */
  def apply(f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean,
      startP: Array[Double], dom: Array[Interval]) = new MDS(f, nbObjectives, compare, startP, dom)
}
