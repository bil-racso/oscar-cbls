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
package oscar.dfo.singleobjective.algos

import oscar.dfo.utils.Interval
import oscar.dfo.singleobjective.SODFOptimizer

/** Framework for the Direct Directional Search (DDS) algorithm used to find the optimum
  * of a derivative free optimization (DFO) problem.
  *
  * @constructor Create a new DDS framework for the problem specified by its function, the
  *              comparison function for evaluations and the domain around the specified
  *              starting point
  * @param f Function to optimize
  * @param nbObjectives The number of objectives to optimize
  * @param compare The comparison function to compare evaluations
  * @param startP The starting point from which the algorithm will begin
  * @param dom The domain of the search space
  * @param proposedBases The bases to be used for the poll step of the DDS algorithm
  * @throws IllegalArgumentException If the starting point doesn't lie within the
  *                                     domain or if the starting point and the domain don't have
  *                                     the same dimensions 
  * @author Cyrille Dejemeppe                                    
  */
class DDS(f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean, startP: Array[Double],
    dom: Array[Interval], proposedBases: Array[Array[Double]]) extends SODFOptimizer {
  
  /** The dimension of the search space */
  val dim = startP.length
  
  //Basic check #1
  for (i <- 0 until dim) {
    if(!(dom(i).isInInterval(startP(i))))
      throw new IllegalArgumentException("The starting point must be in the function domain.")
  }
  
  //Basic check #2
  if(startP.length != dom.length)
    throw new IllegalArgumentException("The starting point and the domain intervals must be of the same dimension.")
  
  /** The point from where we start the algorithm */
  var startPoint = startP
  /** The number of iterations performed */
  var iterCount = 0
  
  /** The index of the last direction tested during the poll step */
  var lastBasis = Array.fill(proposedBases(0).length)(0.0)
  /** Boolean true if the last iteration was declared succesful */
  var lastIterSucc = false
  
  maxAlpha = Array.tabulate(dom.length)(i => dom(i).size / 5.0)
  
  /** The bases or directions used during the poll step */
  var bases = List[Array[Double]]()
  for(i <- 0 until proposedBases.length)
    bases = bases ::: List(Array.tabulate(proposedBases(i).length)(j => proposedBases(i)(j)))
  for(i <- 0 until proposedBases.length)
    bases = bases ::: List(Array.tabulate(proposedBases(i).length)(j => negateBasis(proposedBases(i))(j)))
  
  //Normalizes the initial bases
  for (i <- 0 until bases.length) {
    normalizeBasis(bases(i))
  }
  
  //The step size parameter
  alpha = Array.tabulate(dom.length)(i => dom(i).size / 100.0)
  
  //The evaluation of the starting point
  val evals = evaluator.eval(f, startPoint)
  //The number of function evaluations
  evalCount = evals._2
  /** The current optimum (best point found so far) */
  var x = (startPoint, evals._1)
  
  var currentBest:(Array[Double], Array[Double]) = x
  
  /** The increase step factor */
  val gamma = 2.0
  /** The decrease step factor */
  val beta = 0.5
  
  /** The number of successful iterations in a row */
  var succItCount = 0
  /** The number of unsuccessful iterations in a row */
  var failItCount = 0
  
  /** Reinitializes the values used by the algorithm to their original values */
  def reset = {
    alpha = Array.tabulate(dom.length)(i => dom(i).size / 20.0)
    x = (startPoint, f(startPoint))
    evalCount = 1
    succItCount = 0
  }
  
  /** Normalizes the specified basis.
    *
    * The normalization of the basis is done such that its euclidian norm is unitary.
    * @param b The basis that will be normalized */
  def normalizeBasis(b: Array[Double]) = {
    var sumSquare = 0.0
    for (i <- 0 until b.length)
      sumSquare += b(i) * b(i)
    val norm = math.sqrt(sumSquare)
    for(i <- 0 until b.length)
      b(i) /= norm
  }
  
  /** Places the basis from bases of the specified index at the head of the list
    * in order to ensure it will be tested first during the poll step.
    *
    * @param index The current index of the basis to promote in the list bases */
  def promoteBasis(index: Int) = {
	bases = List(bases(index)) ::: bases.take(index) ::: bases.drop(index + 1)
  }
  
  override def putInDomain(x: Array[Double]): Array[Double] = {
    Array.tabulate(dim)(i => if (dom(i).isInInterval(x(i))) x(i) else dom(i).getClosestBound(x(i)))
  }
  
  /** Returns the basis specified as parameter multiplied by -1.
    *
    * @param basis An array of Double that is the basis to be negated
    * @return An array of Double whose elements are those from the specified
    *         parameter multiplied by -1 */
  def negateBasis(basis: Array[Double]): Array[Double] = {
    Array.tabulate(dim)(i => -basis(i))
  }
  
  /** Returns a pair point value representing the new point created from the current iterate
    * and the specified basis and its evaluations.
    *
    * The new point is created with the following formula: newPoint = x + alpha * basis
    *
    * @param basis An array of Double that is the basis that will be used to create the new point
    * @return A tuple containing two arrays of Double, the first one being the new point and
    *         the second one being its evaluations */
  def newMeshPoint(basis: Array[Double]): (Array[Double], Array[Double]) = {
    val mp = putInDomain(Array.tabulate(dim)(i => x._1(i) + alpha(i) * basis(i)))
    val eval = evaluator.eval(f, mp)
    evalCount += eval._2
    (mp, eval._1)
  }
  
  override def iter(): List[(Array[Double], Array[Double])] = {
    var toRet = List[(Array[Double], Array[Double])]()
    iterCount += 1
    for (i <- 0 until bases.length) {
     if (!areEqual(negateBasis(bases(i)), lastBasis) || !lastIterSucc) {
        val newMPoint = newMeshPoint(bases(i))
		if (compare(newMPoint._2, x._2)) {
		  onImprovement()
		  succItCount += 1
		  failItCount = 0
		  if(succItCount >= 3) {
		    //The iteration was successful 3 times in a row => we increase the step parameter
		    for (k <- 0 until alpha.length)
			  alpha(k) = math.min(gamma * alpha(k), maxAlpha(k))
		    succItCount = 0
		  }
		  lastBasis = Array.tabulate(bases(i).length)(j => bases(i)(j))
		  lastIterSucc = true
		  promoteBasis(i)
		  toRet = toRet ::: List(newMPoint)
		}
      }
    }
    if(toRet.length == 0) {
      //The iteration was unsuccessful => we decrease the step parameter
      for (k <- 0 until alpha.length)
        alpha(k) *= beta
      lastIterSucc = false
      failItCount += 1
      if(failItCount >= 5) {
        //The iteration was unsuccessful 5 times in a row => we add a new basis
	    val newBasis = Array.tabulate(dim)(i  => rand.nextDouble)
	    normalizeBasis(newBasis)
	    bases = bases ::: List(newBasis)
	    failItCount = 0
	  }
      lastBasis = Array.fill(proposedBases(0).length)(0.0)
    }
    toRet
  }
  
  override def simplexDiameter: Double = 0.0
  
  /** Returns the new point found after one iteration of the DDS algorithm around the current iterate.
    *
    * @return A tuple whose two elements are arrays of Double, the first array being the
    *         new point created during the poll step and the second array being its evaluations */
  def iterDDS(): (Array[Double], Array[Double]) = {
    iterCount += 1
    for (i <- 0 until bases.length) {
     if (!areEqual(negateBasis(bases(i)), lastBasis) || !lastIterSucc) {
        val newMPoint = newMeshPoint(bases(i))
		if (compare(newMPoint._2, x._2)) {
		  currentBest = x
		  onImprovement()
		  succItCount += 1
		  failItCount = 0
		  if(succItCount >= 3) {
		    //The iteration was successful 3 times in a row => we increase the step parameter
			for (k <- 0 until alpha.length)
			  alpha(k) = math.min(gamma * alpha(k), maxAlpha(k))
		    succItCount = 0
		  }
		  lastBasis = Array.tabulate(bases(i).length)(j => bases(i)(j))
		  lastIterSucc = true
		  promoteBasis(i)
		  return newMPoint
		}
      }
    }
    //The iteration was unsuccessful => we decrease the step parameter
    for (k <- 0 until alpha.length)
	  alpha(k) *= beta
    lastIterSucc = false
    failItCount += 1
    if(failItCount >= 5) {
      //The iteration was unsuccessful 5 times in a row => we add a new basis
	  val newBasis = Array.tabulate(dim)(i  => rand.nextDouble)
	  normalizeBasis(newBasis)
	  bases = bases ::: List(newBasis)
	  failItCount = 0
	}
    lastBasis = Array.fill(proposedBases(0).length)(0.0)
    x
  }
  
  override def optimize(tol: Double, evalLimit: Int, timeLimit: Int): (Array[Double], Array[Double]) = {
    val begTime = System.currentTimeMillis()
    while (alpha(0) > tol &&  evalCount < evalLimit && (System.currentTimeMillis() - begTime)/1000 < timeLimit) {
      x = iterDDS()
    }
    x
  }
  
  override def sampledOptimize(nbSamples: Int, tol: Double): (Array[Double], Array[Double]) = {
    var fctCallCount = 0
    val samplePoints = sampler.scrambledHaltonSequence(nbSamples, dom)
    var bestResult = (Array.fill(dom.length){0.0}, Array.fill(nbObjectives){0.0})
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

/** Factory for DDS instances. */
object DDS {
  /** Create a DDS according to the specified arguments.
    * 
    * @param f Function to optimize
    * @param nbObjectives The number of objectives to optimize
    * @param compare The comparison function to compare evaluations
    * @param startP The starting point from which the algorithm will begin
    * @param dom The domain of the search space
    * @param proposedBases The bases to be used for the poll step of the DDS algorithm */
  def apply(f: Array[Double] => Array[Double], nbObjectives: Int, compare: (Array[Double], Array[Double]) => java.lang.Boolean, startP: Array[Double], dom:
      Array[Interval], proposedBases: Array[Array[Double]]) = new DDS(f, nbObjectives, compare, startP, dom, proposedBases)
}

