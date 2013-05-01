/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.dfo.algo

import oscar.dfo.utils._
import runtime._


class A[T <: B]

abstract class B {
  val mytab: Array[A[B]]
}


/** Framework for the Nelder-Mead algorithm used to find the optimum
  * of a derivative free optimization (DFO) problem.
  *
  * @constructor Create a new Nelder-Mead framework for the problem specified by its function, the
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
  **/
class NelderMead (f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean, startP: Array[Double], dom: Array[Interval]) extends DFOptimizer {
  
  /** The dimension of the search space */
  val dim = startP.length
  
  /** The starting point of the search */
  var startPoint = startP
  
  //Basic check #1
  for (i <- 0 until dim) {
    if(!(dom(i).isInInterval(startPoint(i))))
      throw new IllegalArgumentException("The starting point must be in the function domain.")
  }
  
  //Basic check #2
  if(startPoint.length != dom.length)
    throw new IllegalArgumentException("The starting point and the domain intervals must be of the same dimension.")
  
  /** The reflection coefficient */
  val deltaR = 1.0
  /** The expansion coefficient */
  val deltaE = 2.0
  /** The inside contraction coefficient */
  val deltaIC = -0.5
  /** The outside contraction coefficient */
  val deltaOC = 0.5
  /** The shrink coefficient */
  val gammaS = 0.5
  
  /** The number of iterations performed by the algorithm */
  var iterCount = 0
  /** The number of successive iterations performed without progress */
  var noProgIterCount = 0
  
  /** The max used for the confidence interval size */
  val max = dom(0).size / 5.0
  
  /** The former best value found */
  var formerBest = Array.fill(nbObjectives){Double.MaxValue}
  
  /** The number of reflections applied */
  var refCount = 0
  /** The number of reflections applied when expansion is considered */
  var refCount2 = 0
  /** The number of expansions applied */
  var expCount = 0
  /** The number of contractions applied */
  var conCount = 0
  /** The number of shrink applied */
  var shrCount = 0
  evalCount = 0
  
  // Initialization of the simplex data structure.
  simplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  
  var currentBest:(Array[Double], Array[Double]) = simplex(0)
  
  /** Fills the simplex data structure with an initial configuration.
    * 
    * Fills the simplex data structure with relevant points: the starting point and
    * other points defined as p_i = the starting point + Width(domain(dim_i)) / 20 */
  def buildSimplex = {
    for (j <- 0 until dim)
      simplex(0) = (startPoint, f(startPoint))
    for (i <- 1 to dim) {
      val tmp = putInDomain(Array.tabulate(dim)(j => if (i == j + 1) startPoint(j) + dom(j).size/20.0 else startPoint(j)))
      simplex(i) = (tmp, f(tmp))
    }
  }
  
  /** Simplex structure of the previous iteration */
  var formerSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  for (i <- 0 to dim) {
    formerSimplex(i) = (Array.ofDim[Double](dim), Array.fill(nbObjectives){0.0})
  }
  
  /** The centroid of the current simplex */
  var centroid = Array.ofDim[Double](dim)
  
  /** Puts all the class variables to their initial values */
  def reset = {
    iterCount = 0
    noProgIterCount = 0
    formerBest = Array.fill(nbObjectives){Double.MaxValue}
    refCount = 0
    refCount2 = 0
    expCount = 0
    conCount = 0
    shrCount = 0
    evalCount = dim + 1
    buildSimplex
    copySimplex
  }
  
  reset
  
  /** Returns the comparison of the former and the new simplices.
    * 
    * @return True if formerSimplex and simplex contains the same points */
  def compareSimplex: Boolean = {
    for (i <- 0 to dim; if (!(formerSimplex(i).equals(simplex(i))))) {
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
    Array.tabulate(dim)(i => if (dom(i).isInInterval(x(i))) x(i) else dom(i).getClosestBound(x(i)))
  }
  
  //Returns the centroid of the current simplex
  /** Returns the centroid of the current simplex.
    * 
    * The centroid of a simplex is defined as follows:
    * y_c = Sum(i <- 0 to dim) y_i / dim.
    *  
    * @return An array of Double that is the centroid of simplex */
  def findCentroid(): Array[Double] = {
    var tmpCentroid = Array.ofDim[Double](dim)
    for (i <- 0 until dim) {
      for (j <- 0 until dim)
        tmpCentroid(j) += simplex(i)._1(j)
    }
    val scaling = 1.0/dim
    for (j <- 0 until dim)
      tmpCentroid(j) *= scaling
    tmpCentroid
  }
  
  /** Returns the reflection of the worst vertex of the simplex around the centroid of the simplex.
    *
    * The reflection of a vertex around the centroid is defined as follows:
    * y_r = y_c + deltaR * (y_c - y_dim).
    * 
    * @param diam The current diameter of the simplex
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return A pair (point, evaluations) that is the reflection of the worst vertex of
    *          the simplex over the centroid of the simplex */
  def reflectPoint(): (Array[Double], Array[Double]) = {
    val rPoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + deltaR * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, rPoint)
    evalCount += eval._2
    (rPoint, eval._1)
  }
  
  /** Returns the expansion of the worst vertex of the simplex around the centroid of the simplex.
    *
    * The expansion of a vertex around the centroid is defined as follows:
    * y_r = y_c + deltaE * (y_c - y_dim).
    * 
    * @param diam The current diameter of the simplex
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return A pair (point, evaluations) that is the reflection of the worst vertex of
    *          the simplex over the centroid of the simplex */
  def expansPoint(): (Array[Double], Array[Double]) = {
    val ePoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + deltaE * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, ePoint)
    evalCount += eval._2
    (ePoint, eval._1)
  }
  
  /** Returns the contraction of the worst vertex of the simplex around the centroid of the simplex.
    *
    * The contraction of a vertex around the best vertex is defined as follows:
    * y_r = y_c + deltaOC * (y_c - y_dim) for an outside contraction
    * y_r = y_c + deltaIC * (y_c - y_dim) for an inside contraction.
    * 
    * @param inside A Boolean that is true if an inside contraction has to be performed and false
    *        if an outside contraction has to be performed
    * @param diam The current diameter of the simplex
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return A pair (point, evaluations) that is the contraction of the worst vertex of
    *          the simplex over the centroid of the simplex */
  def contractPoint(inside: Boolean): (Array[Double], Array[Double]) = {
    evalCount += 1
    val coef = if (inside) deltaIC else deltaOC
    val cPoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + coef * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, cPoint)
    evalCount += eval._2
    (cPoint, eval._1)
  }
  
  /** Shrinks the simplex around the best vertex
    * 
    * Every point y_i in the rotated simplex is defined as follows:
    * y_i = y_0 + gammaS * (y_i - y_0).
    * 
    * @param diam The current diameter of the simplex
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return An array representing a simplex data structure, each vertex of the simplex
    *         being a pair (point, evaluations) that is the shrinking around the first
    *         vertex of simplex. This simplex data structure is sorted in increasing order
    *         according to the compare function.
    */
  def shrink() = {
    for (i <- 1 to dim) {
      evalCount += 1
      val curPoint = Array.tabulate(dim)(j => simplex(0)._1(j) + gammaS * (simplex(i)._1(j) - simplex(0)._1(j)))
      val eval = evaluator.eval(f, curPoint)
      evalCount += eval._2
      simplex(i) = (curPoint, eval._1)
    }
  }
  
  /** Insert an new vertex in simplex (removing the worst vertex and conserving sorted order).
    *
    * @param newPoint The new vertex to be inserted
    */
  def insertInSimplex(newPoint: (Array[Double], Array[Double])) = {
    var tmpToCopy = (newPoint._1, newPoint._2)
    var tmpVal = (newPoint._1, newPoint._2)
    for (i <- 0 to dim - 1) {
      if (compare(newPoint._2, simplex(i)._2)) {
        tmpToCopy = (simplex(i)._1, simplex(i)._2)
        simplex(i) = (tmpVal._1, tmpVal._2)
        tmpVal = (tmpToCopy._1, tmpToCopy._2)
      }
    }
    simplex(dim) = (tmpVal._1, tmpVal._2)
  }
  
  def simplexDiameter: Double = {
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
    if(iterNM()) {
      scala.util.Sorting.stableSort(simplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue)
      var i = 0
      while (i < simplex.length && !areEqual(simplex(i)._1, formerBest._1)) {
        toRet = toRet ::: List(simplex(i))
        i += 1
      }
    }
    toRet
  }
  
  /** Performs one iteration of the Nelder-Mead algorithm and returns a Boolean indicating if the simplex need to be re-sorted.
    *
    * @return A Boolean that is True if the simplex need to be re-sorted, false otherwise */
  def iterNM(): Boolean = {
    //Recompute the centroid for the new simplex.
    centroid = findCentroid()
    //Compute the reflection point.
    val rPoint = reflectPoint()
    val evalRPoint = rPoint._2
    val evalWorst = simplex(dim)._2
    val evalAlmostWorst = simplex(dim - 1)._2
    val evalBest = simplex(0)._2
    //Case where the reflection point represent an amelioration but is not better than the best point in the simplex
    if ((compare(evalBest, evalRPoint) || (!compare(evalBest, evalRPoint) && !compare(evalRPoint, evalBest))) && compare(evalRPoint, evalAlmostWorst)) {
      insertInSimplex(rPoint)
      refCount += 1
      return false
    }
    //Case where the reflection point is better than the current best point in the simplex.
    else if (compare(evalRPoint, evalBest)) {
      //Compute the expansion point
      val ePoint = expansPoint()
      //Case where the expansion point is better than the reflection point.
      if (compare(ePoint._2, evalRPoint)) {
        insertInSimplex(ePoint)
        expCount += 1
        return false
      }
      //Case where the expansion point is not better than the reflection point.
      else {
        insertInSimplex(rPoint)
        refCount2 += 1
        return false
      }
    }
    //Case where the reflection point is worse than the one before worst point of the simplex
    else if(!compare(evalRPoint, evalAlmostWorst)) {
      //We perform an inside contraction
      if (!compare(evalRPoint, evalWorst)) {
        val iCPoint = contractPoint(true)
        if (compare(iCPoint._2, evalWorst)) {
          insertInSimplex(iCPoint)
          conCount += 1
          return false
        }
        else {
          shrink()
          shrCount += 1
          return true
        }
      }
      //We perform an outside contraction
      else {
        val oCPoint = contractPoint(false)
        if (compare(oCPoint._2, evalRPoint)) {
          insertInSimplex(oCPoint)
          conCount += 1
          return false
        }
        else {
          shrink()
          shrCount += 1
          return true
        }
      }
    }
    true
  }
  
  override def optimize(tol: Double, evalLimit: Int, timeLimit: Int): (Array[Double], Array[Double]) = {
    var needToSort = true
    var stop = false
    val begTime = System.currentTimeMillis()
    while (!stop) {
      if(needToSort) {
        //Sort the array such that the points are sorted in the increasing order of their function evaluation.
        scala.util.Sorting.stableSort(simplex, (pvp1: (Array[Double], Array[Double]), pvp2: (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue)
        needToSort = false
      }
      if (simplexDiameter < tol) {
        return simplex(0)
      }

      needToSort = iterNM()
      iterCount +=1
      //Check if the last iteration brought some progress.
      if (compareSimplex || evalCount >= evalLimit || (System.currentTimeMillis() - begTime)/1000 >= timeLimit) {
    	  return simplex(0)
      }
      copySimplex
      if (compare(simplex(0)._2, formerBest)) {
    	  currentBest = simplex(0)  
          onImprovement()
    	  formerBest = simplex(0)._2
    	  noProgIterCount = 0
      }
      else {
    	  noProgIterCount +=1
      }
      if (noProgIterCount >= dim * 10) {
    	  return simplex(0)
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
      val curResult = optimize(tol, 10000, 100)
      fctCallCount += evalCount
      if(compare(curResult._2, bestResult._2))
        bestResult = (curResult._1, curResult._2)
    }
    evalCount = fctCallCount
    bestResult
  }
}

/** Factory for NelderMead instances. */
object NelderMead {
  /** Create a NelderMead according to the specified arguments.
    * 
    * @param f Function to optimize
    * @param nbObjectives The number of objectives to optimize
    * @param compare The comparison function to compare evaluations
    * @param startP The starting point from which the algorithm will begin
    * @param dom The domain of the search space */
  def apply(f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean,
      startP: Array[Double], dom: Array[Interval]) = new NelderMead(f, nbObjectives, compare, startP, dom)
}
