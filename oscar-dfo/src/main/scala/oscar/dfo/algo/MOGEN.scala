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

/** Framework for the MOGEN algorithm used to find the Pareto set of a multiobjective
  * derivative free optimization (MODFO) problem using single objective optimization
  * algorithms.
  *
  * @constructor Create a new MOGEN framework for the problem specified by its objective
  *              functions, its objective bounds and the domain of the search space
  * @param objFunctions The objective functions for which we want to find the Pareto set
  * @param objBounds The maximal values for the objectives
  * @param method A String that represents the single objective algorithm to use (MDS, NM or DDS)
  * @param dom The domain of the search space 
  * @author Cyrille Dejemeppe
  */
class MOGEN (objFunctions: Array[Double] => Array[Double], objBounds: Array[Double], method: String, dom: Array[Interval]) extends MODFOptimizer {
  val nbObjectives = objBounds.length
  
  val dim = dom.length
  
  boundsOK = false
  
  evalCount = 0
  
   /** Erases points which do not lie within the objective bounds from the archive. */
  def cleanArchive = {
    var newList = List[(Array[Double], Array[Double], DFOptimizer)]()
    for (j <- 0 until archive.length) {
      if (withinBounds(archive(j)._2, true, objBounds))
        newList = newList ::: List(archive(j))
    }
    archive = newList
  }
  
  /** Archive containing points that still need to be refined */
  var archive = List[(Array[Double], Array[Double], DFOptimizer)]()
  
  /** The number of successive failed iterations */
  var failedIter = 0
  
  /** Returns an array that is the archive sorted in increasing order of the objective value of the first objective function
    *
    * @return An array that is the archive sorted in increasing order of the objective value of the first
    *         objective function*/
  def sortedArchive: Array[(Array[Double], Array[Double], DFOptimizer)] = {
    var sortedArchive = archive.toArray
    scala.util.Sorting.stableSort(sortedArchive, (e1: (Array[Double], Array[Double], DFOptimizer), e2: (Array[Double], Array[Double], DFOptimizer)) => e1._2(0) < e2._2(0))
    sortedArchive
  }
  
  def getArchive: List[(Array[Double], Array[Double])] = {
    var pairArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      pairArch = pairArch ::: List((archive(i)._1, archive(i)._2))
    pairArch
  }
  
  /** Initializes the archive with the specified number of points generated from the specified method.
    * 
    * @param nbPoints An Int representing the number of points to be generated and put in the archive
    * @param method A string representing the method used to generate points:
    *        1, nLine, nRand or nQuasiRand */
  def initialization(nbPoints: Int, meth: String) = {
    meth match {
      //We take a single point in the middle of the domain as initial archive
      case "1" => {
        val newPoint = Array.tabulate(dom.length)(i => dom(i).min + (dom(i).size / 2))
        archive = List(createNewPoint(newPoint, objFunctions(newPoint)))
        evalCount += 1
      }
      //We take n points equally spaced on the line connecting l and u (being the lower and the upper bounds of the domain) as initial archive
      case "nLine" => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (nbPoints - 1).toDouble) * dom(j).size))
        for (i <- 0 until nbPoints) {
          val evals = objFunctions(newPoints(i))
          updateArchive((newPoints(i), evals))
        }
        evalCount += nbPoints
      }
      //We take n points uniformly randomly generated on the domain as initial archive
      case "nRand" => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(i => dom(i).min + (rand.nextDouble * dom(i).size))
        for (i <- 0 until nbPoints)
          updateArchive((newPoints(i), objFunctions(newPoints(i))))
        evalCount += nbPoints
      }
      //We take n points generated with a quasi-random sequence on the domain as initial archive
      case "nQuasiRand" => {
        val newPoints = sampler.scrambledHaltonSequence(nbPoints, dom)
        for (i <- 0 until newPoints.length)
          updateArchive((newPoints(i), objFunctions(newPoints(i))))
        evalCount += nbPoints
      }
      //Default case: see "nLine"
      case _ => {
        val newPoints = Array.fill(nbPoints){Array.fill(dom.length){0.0}}
        for (i <- 0 until nbPoints)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (nbPoints - 1).toDouble) * dom(j).size))
        for (i <- 0 until nbPoints)
          updateArchive((newPoints(i), objFunctions(newPoints(i))))
        evalCount += nbPoints
      }
    }
  }
  
  /** Returns a triple (point, evaluations, DFOptimizer) to be put in the archive according to the specified parameters
    * 
    * @param point An array of Double being the point from which we create the triple 
    * @param evals An array of Double being the evaluations of the point from which we create the triple
    * @param simplex A simplex data structure that will be the simplex data structure of the DFOptimzer associated to the
    *        point
    * @return A triple (point, evaluations, DFOptimizer) where the point and evaluations and the one passed as arguments
    *         and DFOptimizer is a DFOptimizer generated around the point with the simplex specified as argument if not null */
  def createNewPoint(point: Array[Double], evals: Array[Double], simplex: Array[(Array[Double], Array[Double])] = null): (Array[Double], Array[Double], DFOptimizer) = {
    val solver = method match {
      case "NM" => NelderMead(objFunctions, 1, compareDominance, point, dom)
      case "DDS" => DDS(objFunctions, 1, compareDominance, point, dom, Array.tabulate(dom.length)(i => Array.tabulate(dom.length)(j => if (i == j) 1.0 else 0.0)))
      case "MDS" => MDS(objFunctions, 1, compareDominance, point, dom)
      case _ => MDS(objFunctions, 1, compareDominance, point, dom)
    }
    if (method != "DDS" && simplex != null) {
      solver.simplex = simplex
    }
    (point, evals, solver)
  }
  
  /** Returns true if evalsP1 is not dominated by evalsP2. This is the comparison function used
    * by the single objective algorithm.
    * 
    * @param evalsP1 An array of Double representing the evaluations of a point
    * @param evalsP2 An array of Double representing the evaluations of a point
    * @return A Boolean that is true if evalsP1 is not dominated by evalsP2 */
  def compareDominance(evalsP1: Array[Double], evalsP2: Array[Double]): Boolean = {
    val comp = compareSolution(evalsP1, evalsP2, 0)
    return comp._2 > 0
  }

  /** Updates the archive with respect to p and returns true if it has changed, false otherwise.
    * 
    * Adds p in the archive if it isn't dominated by any point in the archive. Removes the
    * points dominated by p from the archive.
    * 
    * @param p A pair (point, evaluations) to be added in the archive
    * @param simplex A simplex data structure from which the point was obtained if not null
    * @return A Boolean that is true if the archive has changed, false otherwise */
  def updateArchive(p: (Array[Double], Array[Double]), simplex: Array[(Array[Double], Array[Double])] = null): Boolean = {
    var listArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      listArch = listArch ::: List((archive(i)._1, archive(i)._2))
    if (!inArchive(p._2, listArch) && withinBounds(p._2, boundsOK, objBounds)) {
      var stop = false
      var j = 0
      while(j < archive.length && !stop) {
        val comp = compareSolution(p._2, archive(j)._2, archive(j)._3.simplexDiameter + average(archive(j)._3.alpha))
        if (comp._1 != nbObjectives) {
          if (comp._1 + comp._2 == nbObjectives) {
            archive = archive.take(j) ::: archive.drop(j+1)
            j -= 1
          }
          else if (comp._1 + comp._3 == nbObjectives) {
            stop = true
            return false
          }
        }
        j += 1
      }
      if (!stop) {
        archive = archive ::: List(createNewPoint(p._1, p._2, simplex))
      }
    }
    true
  }
  
  /** Returns the iterate to be chosen for the next iteration of the algorithm.
    * 
    * @param iteration An Int that is the number of the current iteration 
    * @return A triple (point, evaluations, DFOptimizer) that is the element of the
    *         archive that will be used for an iteration of the MOGEN algorithm */
  def selectIterate(iteration: Int): (Array[Double], Array[Double], DFOptimizer) = {
    if(failedIter >= 5) {
      archive = archive.drop(1) ::: archive.take(1)
      return archive(0)
    }
    val sortedArch = sortedArchive
    var listArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      listArch = listArch ::: List((archive(i)._1, archive(i)._2))
    val scaleObj = scaleObjectives(listArch)
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
        if (sortedArch(i)._3.simplexDiameter + average(sortedArch(i)._3.alpha) > iterate._3.simplexDiameter + average(iterate._3.alpha))
          iterate = sortedArch(i)
      }
    }
    iterate
  }
  
   /** The optional search step that tries some new points around the specified iterate and
     * updates the archive if necessary.
     * 
     * @param iterate A triple (point, evaluations, alpha) that is an element of the archive
     * @param iteration The number of the current iteration */
  def searchStep(iterate: (Array[Double], Array[Double], DFOptimizer), iteration: Int) = {
    if (archive.length > 1 && iteration % 10 != 0) {
      val randIndex = rand.nextInt(archive.length)
      val randPoint = archive(randIndex)
      if (!areEqual(iterate._2, randPoint._2)) {
        val randDistance = 0.25 + (rand.nextDouble * 0.5)
        val newPoint = Array.tabulate(iterate._1.length)(i => iterate._1(i) + (randDistance * (randPoint._1(i) - iterate._1(i))))
        if (inDomain(newPoint, dom)) {
          val evals = evaluator.eval(objFunctions, newPoint)
          evalCount += evals._2
          updateArchive((newPoint, evals._1))
        }
      }
    }
    else {
      val newPoint = Array.tabulate(iterate._1.length)(i => dom(i).min + rand.nextDouble * (dom(i).max - dom(i).min))
      val evals = evaluator.eval(objFunctions, newPoint)
      evalCount += evals._2
      updateArchive((newPoint, evals._1))
    }
  }
  
  override def findPareto(nbPoints: Int, tol: Double): List[(Array[Double], Array[Double])] = {
    //Initializes the archive
    initialization(25, "nLine")
    nbIter = 0
    var successfulIter = false
    while(archive.length < nbPoints && evalCount < 5000) {
      successfulIter = false
      val iterate = selectIterate(nbIter)
      searchStep(iterate, nbIter)
      val formerEvalCount = iterate._3.evalCount
      val newPoints = iterate._3.iter()
      if (newPoints.length == 0) {
        failedIter += 1
        archive = List((archive(0)._1, archive(0)._2, archive(0)._3)) ::: archive.drop(1)
      }
      evalCount += (iterate._3.evalCount - formerEvalCount)
      val newSimplex = iterate._3.simplex
      //Reseting the simplex with the iterate as leading vertex.
      if(method != "DDS") {
        var index = -1
        var i = 0
        while (index < 0 && i < newSimplex.length) {
          if (areEqual(iterate._1, newSimplex(i)._1)) {
            index = i
          }
          i += 1
        }
        val iterateSimplex = Array.tabulate(newSimplex.length)(j => if(j == 0) (iterate._1, iterate._2) else if (j == index) newSimplex(0) else newSimplex(j))
        archive(0)._3.simplex = iterateSimplex
      }
      archive = archive.drop(1) ::: archive.take(1)
      //Adding the new points in the archive
      for (i <- 0 until newPoints.length) {
        if(method != "DDS") {
          var index = -1
          var k = 0
          while (index < 0 && k < newSimplex.length) {
            if (areEqual(newPoints(k)._1, newSimplex(k)._1)) {
              index = k
            }
            k += 1
          }
          val npSimplex = Array.tabulate(newSimplex.length)(j => if(j == 0) (newPoints(i)._1, newPoints(i)._2) else if (j == index) newSimplex(0) else newSimplex(j))
    	  if (updateArchive(newPoints(i), npSimplex))
    	    successfulIter = true
        }
        else if (method == "DDS") {
          if (updateArchive(newPoints(i), null))
            successfulIter = true
        }
      }
      if (successfulIter)
        failedIter = 0
      else
        failedIter += 1
      //Cleans the archive if necessary
      if (!boundsOK) {
        var i = 0
        while (!boundsOK && i < archive.length) {
          if (withinBounds(archive(i)._2, true, objBounds)) {
            cleanArchive
            boundsOK = true
          }
          i += 1
        }
      }
      nbIter += 1
    }
    var pareto = List[(Array[Double], Array[Double])]()
    for (i <- 0 until archive.length)
      pareto = pareto ::: List((archive(i)._1, archive(i)._2))
    pareto
  }
}

/** Factory for MOGEN instances. */
object MOGEN {
  /** Create a MOGEN according to the specified arguments.
    * 
    * @param objFunctions The objective functions for which we want to find the Pareto set
    * @param objBounds The maximal values for the objectives
    * @param method A String that represents the single objective algorithm to use (MDS, NM or DDS)
    * @param dom The domain of the search space */
  def apply (objFunctions: Array[Double] => Array[Double], objBounds: Array[Double], method: String, dom: Array[Interval]) = new MOGEN(objFunctions, objBounds, method, dom)
}
