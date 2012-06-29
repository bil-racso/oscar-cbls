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
 ******************************************************************************/
package oscar.dfo.algo

import oscar.dfo.utils._

/** Framework for the DMS algorithm used to find the Pareto set of a multiobjective
  * derivative free optimization (MODFO) problem.
  *
  * @constructor Create a new FormerDMS framework for the problem specified by its objective
  *              functions, its objective bounds and the domain of the search space
  * @param objectiveFunctions The objective functions for which we want to find the Pareto set
  * @param objectiveBounds The maximal values for the objectives
  * @param dom The domain of the search space
  * @param proposedBases The bases used for the poll step 
  * @author Cyrille Dejemeppe 
  */
class FormerMODMS (objectiveFunctions: Array[Double] => Array[Double], objectiveBounds: Array[Double], dom: Array[Interval], proposedBases: Array[Array[Double]]) extends MODFOptimizer {
  
  val nbObjectives = objectiveBounds.length
  
  evalCount = nbObjectives
  
  boundsOK = false
  
  /** The number of successive iterations without any improvement */
  var noImprovement = 0
  
  /** the maximal value for alpha */
  val maxAlpha = Array.tabulate(dom.length)(i => dom(i).size / 5.0)
  
  /** The beta factor used to decrease the step parameter */
  val beta = 0.5
  /** The gamma factor used to increase the step parameter */
  val gamma = 2.0
  
  boundsOK = false
  val dim = dom.length
  
  /** The bases used to create new mesh points during the poll step */
  var bases = List[Array[Double]]()
  
  //Initialization of the bases
  for (i <- 0 until proposedBases.length) {
    bases = bases ::: List(normalize(proposedBases(i)))
    bases = bases ::: List(normalize(negateBasis(proposedBases(i))))
  }

  /** The archive that will contain the current approximation of the Pareto set */
  var archive = List[(Array[Double], Array[Double], Array[Double])]()
  
  /** Function used to restart the search with original bases and original archive.
    * 
    * bases is set to proposedBases while the archive is set to contain only the start point. */
  def reset = {
    bases = List[Array[Double]]()
    for (i <- 0 until proposedBases.length) {
      bases = bases ::: List(normalize(proposedBases(i)))
      bases = bases ::: List(normalize(negateBasis(proposedBases(i))))
    }
    initialization(25, "nLine")
  }
  
  def getArchive: List[(Array[Double], Array[Double])] = {
    var pairArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      pairArch = pairArch ::: List((archive(i)._1, archive(i)._2))
    pairArch
  }
  
  /** Returns an array that is the archive sorted in increasing order of the objective value of the first objective function
    *
    * @return An array that is the archive sorted in increasing order of the objective value of the first
    *         objective function*/
  def sortedArchive: Array[(Array[Double], Array[Double], Array[Double])] = {
    var sortedArchive = archive.toArray
    scala.util.Sorting.stableSort(sortedArchive, (e1: (Array[Double], Array[Double], Array[Double]), e2: (Array[Double], Array[Double], Array[Double])) => e1._2(0) < e2._2(0))
    sortedArchive
  }
  
  /** Returns a vector that is the input vector normalized (i.e. its euclidian norm is unitary).
    * 
    * @param vector An array of Double that is the vector to be normalized
    * @return An array of Double that is the normalized input vector */
  def normalize(vector: Array[Double]): Array[Double] = {
    var sumNorm = 0.0
    val newVector = Array.tabulate(vector.length)(i => vector(i))
    for (i <- 0 until vector.length)
      sumNorm += math.pow(vector(i), 2)
    sumNorm = math.sqrt(sumNorm)
    for (i <- 0 until vector.length)
      newVector(i) /= sumNorm
    newVector
  }
  
  /** Initializes the archive with the specified number of points generated from the specified method.
    * 
    * @param nbPoints An Int representing the number of points to be generated and put in the archive
    * @param method A string representing the method used to generate points:
    *        1, nLine, nRand or nQuasiRand */
  def initialization(nbPoints: Int, method: String) = {
    val alphaInit = Array.tabulate(dom.length)(i => dom(i).size / 20.0)
    method match {
      //We take a single point in the middle of the domain as initial archive
      case "1" => {
        val newPoint = Array.tabulate(dom.length)(i => dom(i).min + (dom(i).size / 2))
        archive = List((newPoint, objectiveFunctions(newPoint), alphaInit))
        evalCount += 1
      }
      //We take n points equally spaced on the line connecting l and u (being the lower and the upper bounds of the domain) as initial archive
      case "nLine" => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (nbPoints - 1).toDouble) * dom(j).size))
        for (i <- 0 until nbPoints) {
          val evals = objectiveFunctions(newPoints(i))
          updateArchive((newPoints(i), evals), alphaInit)
        }
        evalCount += nbPoints
      }
      //We take n points uniformly randomly generated on the domain as initial archive
      case "nRand" => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(i => dom(i).min + (rand.nextDouble * dom(i).size))
        for (i <- 0 until nbPoints)
          updateArchive((newPoints(i), objectiveFunctions(newPoints(i))), alphaInit)
        evalCount += nbPoints
      }
      //We take n points generated with a quasi-random sequence on the domain as initial archive
      case "nQuasiRand" => {
        val newPoints = sampler.scrambledHaltonSequence(nbPoints, dom)
        for (i <- 0 until newPoints.length)
          updateArchive((newPoints(i), objectiveFunctions(newPoints(i))), alphaInit)
        evalCount += nbPoints
      }
      //Default case: see "nLine"
      case _ => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (nbPoints - 1).toDouble) * dom(j).size))
        for (i <- 0 until nbPoints)
          updateArchive((newPoints(i), objectiveFunctions(newPoints(i))), alphaInit)
        evalCount += nbPoints
      }
    }
  }
  
  /** Returns the basis specified as parameter multiplied by -1.
    *
    * @param basis An array of Double that is the basis to be negated
    * @return An array of Double whose elements are those from the specified
    *         parameter multiplied by -1 */
  def negateBasis(basis: Array[Double]): Array[Double] = {
    Array.tabulate(basis.length)(i => -basis(i))
  }
  
  /** Returns a pair (Point, Evaluations) representing the new point created from the basis and the point.
    *
    * The new point is generated according to the following formula:
    * new point = former point + alpha * basis
    * 
    * @param point An array of Double being the point from which the new point will be created
    * @param basis An array of Double representing the direction in which the new point will be
    *        created from the specified point
    * @param alpha An array of Double representing the step size used to create the new point
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return A pair (point, evaluations) being the new point created from the specified input
    *         and the evaluations of the objective functions at this point */
  def newMeshPoint(point: Array[Double], basis: Array[Double], alpha: Array[Double], tol: Double): (Array[Double], Array[Double]) = {    
    val mp = putInDomain(Array.tabulate(point.length)(i => point(i) + alpha(i) * basis(i)), dom)
    val ans = evaluator.eval(objectiveFunctions, mp)
    evalCount += ans._2
    val evals = ans._1
    (mp, evals)
  }
  
  /** Returns a list of pairs (Point, Evaluations) representing the new points created from the bases and the point.
    *
    * The new points are generated according to the following formula:
    * new point = former point + alpha * basis
    * 
    * @param point An array of Double being the point from which the new point will be created
    * @param alpha An array of Double representing the step size used to create the new point
    * @param tol The tolerance used to specify the level of precision we want to obtain
    * @return A list of pairs (point, evaluations) being the new point created from the specified
    *         input and the evaluations of the objective functions at this point */
  def createNewPoints(point: Array[Double], alpha: Array[Double], tol: Double): List[(Array[Double], Array[Double])] = {
    var newP = List[(Array[Double], Array[Double])]()
    for (i <- 0 until bases.length) {
      newP = newP ::: List(newMeshPoint(point, bases(i), alpha, tol))
    }
    newP
  }
  
  /** Updates the archive wrt p, returns true if p is added to the archive, false otherwise.
    * 
    * @param p A pair (point, evaluations) being the point to be added in the current archive
    * @param curAlpha The step parameter that was used to create p
    * @return A Boolean that is true if p has been added to the archive, false otherwise */
  def updateArchive(p: (Array[Double], Array[Double]), curAlpha: Array[Double]): Boolean = {
    var added = false
    var j = 0
    while(j < archive.length) {
      val comp = compareSolution(p._2, archive(j)._2, average(curAlpha))
      if (comp._1 != nbObjectives) {
        if (comp._1 + comp._2 == nbObjectives) {
          archive = archive.take(j) ::: archive.drop(j+1)
          j -= 1
        }
        else if (comp._1 + comp._3 == nbObjectives)
          return false
      }
      j += 1
    }
    archive = archive ::: List((p._1, p._2, Array.tabulate(dom.length)(i => math.min(gamma * curAlpha(i), maxAlpha(i)))))
    true
  }
  
  /** Erases points which do not lie within the objective bounds from the archive. */
  def cleanArchive = {
    var newList = List[(Array[Double], Array[Double], Array[Double])]()
    for (j <- 0 until archive.length) {
      if (withinBounds(archive(j)._2, true, objectiveBounds))
        newList = newList ::: List(archive(j))
    }
    archive = newList
  }
  
  /** Returns the iterate to be chosen for the next iteration of the algorithm.
    * 
    * @param iteration An Int that is the number of the current iteration 
    * @return A triple (point, evaluations, DFOptimizer) that is the element of the
    *         archive that will be used for an iteration of the MOGEN algorithm */
  def selectIterate(iteration: Int): (Array[Double], Array[Double], Array[Double]) = {
    if(noImprovement >= 5) {
      archive = archive.drop(1) ::: archive.take(1)
      return archive(0)
    }
    val sortedArch = sortedArchive
    var arch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      arch = arch ::: List((archive(i)._1, archive(i)._2))
    val scaleObj = scaleObjectives(arch)
    var maxDist = Double.MinValue
    var iterate = sortedArch(0)
    for (i <- 0 until sortedArch.length - 1) {
      val curDist = computeScaledDistance(sortedArch(i)._2, sortedArch(i + 1)._2, scaleObj)
      if (maxDist < curDist) {
        maxDist = curDist
        iterate = if (iteration %2 == 0) sortedArch(i) else sortedArch(i + 1)
      }
      //When there is a tie break, the point with the biggest step size is chosen
      else if (maxDist == curDist) {
        if (average(sortedArch(i)._3) > average(iterate._3))
          iterate = sortedArch(i)
      }
    }
    iterate
  }
  
  /** The optional search step that tries some new points around the specified iterate
    * and updates the archive if necessary.
    * 
    * @param iterate A triple (point, evaluations, alpha) that is an element of the archive */
  def searchStep(iterate: (Array[Double], Array[Double], Array[Double])) = {
    if (archive.length > 1) {
      val randIndex = rand.nextInt(archive.length)
      val randPoint = archive(randIndex)
      if (!areEqual(iterate._2, randPoint._2)) {
        val randDistance = 0.25 + (rand.nextDouble * 0.5)
        val newPoint = Array.tabulate(iterate._1.length)(i => iterate._1(i) + (randDistance * (randPoint._1(i) - iterate._1(i))))
        if (inDomain(newPoint, dom)) {
          val evals = evaluator.eval(objectiveFunctions, newPoint)
          evalCount += evals._2
          updateArchive((newPoint, evals._1),  Array.tabulate(iterate._3.length)(i => dom(i).size/20.0))
        }
      }
    }
  }
  
  /** Performs one iteration of the DMS algorithm.
    * 
    * @param tol The tolerance used to specify the level of precision we want to obtain */
  def iterMODMS(tol: Double) = {
    //Iterate selection
    val it = selectIterate(nbIter)
    //Search step
    searchStep(it)
    val curPoint = it._1
    val curAlpha = it._3
    val newPoints = createNewPoints(curPoint, curAlpha, tol)
    var success = false
    //Updating the archive wrt the new points.
    var archiveList = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      archiveList = archiveList ::: List((archive(i)._1, archive(i)._2))
    for (i <- 0 until newPoints.length) {
      if (!inArchive(newPoints(i)._2, archiveList) && withinBounds(newPoints(i)._2, boundsOK, objectiveBounds)) {
        if (updateArchive(newPoints(i), curAlpha))
          success = true
      }
    }
    //Updates the step size parameter of the point used as iterate.
    if (areEqual(archive(0)._1, curPoint)) {
      if (success)
        archive = List((archive(0)._1, archive(0)._2, Array.tabulate(dom.length)(k => math.min(gamma * curAlpha(k), maxAlpha(k))))) ::: archive.drop(1)
      else
        archive = List((archive(0)._1, archive(0)._2, Array.tabulate(dom.length)(k => beta * curAlpha(k)))) ::: archive.drop(1)
      //archive = archive.drop(1) ::: archive.take(1)
    }
    if (success)
      noImprovement = 0
    else
      noImprovement += 1
    //Adding a new random basis to try refining the set of directions if there were too many unsuccessful iterations.
    if (noImprovement >= 10) {
      val newBasis = normalize(Array.tabulate(bases(0).length)(i => rand.nextDouble))
      bases = bases ::: List(newBasis)
      noImprovement = 0
    }
  }
  
  override def findPareto(nbPoints: Int, tol: Double): List[(Array[Double], Array[Double])] = {
    initialization(25, "nLine")
    while (archive.length < nbPoints && evalCount < 1000) {
      //Checks if there are points in the archive that are not lying within the objective bounds and need to be erased.
      if (!boundsOK) {
        var i = 0
        while (!boundsOK && i < archive.length) {
          if (withinBounds(archive(i)._2, true, objectiveBounds)) {
            cleanArchive
            boundsOK = true
          }
          i += 1
        }
      }
      nbIter += 1
      iterMODMS(tol)
    }
    var pareto = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      pareto = pareto ::: List((archive(i)._1, archive(i)._2))
    pareto
  }
}

/** Factory for FormerMODMS instances. */
object FormerMODMS {
  /** Create a FormerMODMS according to the specified arguments.
    * 
    * @param objectiveFunctions The objective functions for which we want to find the Pareto set
    * @param objectiveBounds The maximal values for the objectives
    * @param startP An array of Double being the coordinates of the starting point
    * @param dom The domain of the search space
    * @param proposedBases The bases used for the poll step */
  def apply(objectiveFunctions: Array[Double] => Array[Double], objectiveBounds: Array[Double], dom: Array[Interval],
      proposedBases: Array[Array[Double]]) = new FormerMODMS(objectiveFunctions, objectiveBounds, dom, proposedBases)
}
