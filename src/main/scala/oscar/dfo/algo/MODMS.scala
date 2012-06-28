/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package oscar.dfo.algo

import oscar.dfo.utils._

/** Framework for the DMS algorithm used to find the Pareto set of a multiobjective
  * derivative free optimization (MODFO) problem.
  *
  * @constructor Create a new MODMS framework for the problem specified by its objective
  *              functions, its objective bounds and the domain of the search space
  * @param objFunctions The objective functions for which we want to find the Pareto set
  * @param objBounds The maximal values for the objectives
  * @param dom The domain of the search space
  * @param proposedBases The bases used for the poll step 
  * @author Cyrille Dejemeppe  
  */
class MODMS (objFunctions: Array[Double] => Array[Double], objBounds: Array[Double], dom: Array[Interval], proposedBases: Array[Array[Double]]) extends MODFOptimizer {
  
  val dim = dom.length
  
  /** The number of successive failed iterations */
  var failedIter = 0
  
  val nbObjectives = objBounds.length
  
  /** The list containing the current approximation of the Pareto set (point, evals, alpha) */
  var lk = List[(Array[Double], Array[Double], Array[Double])]()
  
  /** The list of the bases (the positive spanning set) */
  var d = List[Array[Double]]()
  
  /** The increasing step size parameter */
  var gamma = 2.0
  
  /** The decreasing step parameter */
  var beta = 0.5
  
  /** Returns lk sorted in increasing order of the objective value of the first objective function.
    * 
    * @return An array of triple (point, evaluations, DFOptimizer) containing the elements of lk
    *         such as they are sorted in increasing order of the first objective function evaluation */
  def sortLk: Array[(Array[Double], Array[Double], Array[Double])] = {
    var sortedLk = lk.toArray
    scala.util.Sorting.stableSort(sortedLk, (e1: (Array[Double], Array[Double], Array[Double]), e2: (Array[Double], Array[Double], Array[Double])) => e1._2(0) < e2._2(0))
    sortedLk
  }
  
  /** Returns true if p is in the domain, false otherwise.
    *
    * @param p: An array of Double being the point we want to know whether it is lying in the domain
    * @return A Boolean that is true if p is within the domain, false otherwise */
  def inDomain(p: Array[Double]): Boolean = {
    for (i <- 0 until p.length) {
      if (!dom(i).isInInterval(p(i)))
        return false
    }
    true
  }
  
  def getArchive: List[(Array[Double], Array[Double])] = {
    var pairArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until lk.length)
      pairArch = pairArch ::: List((lk(i)._1, lk(i)._2))
    pairArch
  }
  
  /** Returns the basis specified as parameter multiplied by -1.
    *
    * @param basis An array of Double that is the basis to be negated
    * @return An array of Double whose elements are those from the specified
    *         parameter multiplied by -1 */
  def negateVector(vector: Array[Double]): Array[Double] = {
    Array.tabulate(vector.length)(i => -1.0 * vector(i))
  }
  
  /** Returns true if evals1 dominates evals2, false otherwise
    * 
    * @param evals1 A triple (point, evaluations, step parameter) to be compared with evals2
    * @param evals2 A triple (point, evaluations, step parameter) to be compared with evals1
    * @return A Boolean that is true if evals1 dominates evals2, false otherwise */
  def dominates(evals1: (Array[Double], Array[Double], Array[Double]), evals2: (Array[Double], Array[Double], Array[Double])): Boolean = {
    var archlk = List[(Array[Double], Array[Double])]()
    for (i <- 0 until lk.length)
      archlk = archlk ::: List((lk(i)._1, lk(i)._2))
    val scaleObj = scaleObjectives(archlk)
    for (i <- 0 until evals1._2.length) {
      if ((evals2._2(i) < (evals1._2(i)  - rho(average(evals1._3)))) && ((evals2._2(i) + rho(average(evals2._3))) < evals1._2(i)))
        return false
    }
    true
  }
  
  /** Returns the list l without the point p
    * 
    * @param p A triple (point, evaluations, step parameter) we want to remove from l
    * @param l A list of triple (point, evaluations, step parameter) from which we want
    *        to remove p
    * @return A list of triple (point, evaluations, step parameter) from which p has been removed */
  def remove(p: (Array[Double], Array[Double], Array[Double]), l: List[(Array[Double], Array[Double], Array[Double])]): List[(Array[Double], Array[Double], Array[Double])] = {
    var lRet = List[(Array[Double], Array[Double], Array[Double])]()
    for (i <- 0 until l.length) {
      if (!areEqual(p._1, l(i)._1))
        lRet = lRet ::: List(l(i))
    }
    lRet
  }
  
  /** Returns true if p is in the list l
    * 
    * @param p A triple (point, evaluations, step parameter) we want to test it is in l
    * @param l A list of triple (point, evaluations, step parameter) from which we want
    *        to know whether p belongs
    * @return A Boolean that is true if p is in l, false otherwise */
  def inList(p: (Array[Double], Array[Double], Array[Double]), l: List[(Array[Double], Array[Double], Array[Double])]): Boolean = {
    for (i <- 0 until l.length) {
      if (areEqual(p._2, l(i)._2))
        return true
    }
    false
  }
  
  /** Returns true if l1 and l2 contain the same elements
    * 
    * @param l1 A list of triple (point, evaluations, step parameter) 
    * @param l2 A list of triple (point, evaluations, step parameter)
    * @return A Boolean that is true if l1 and l2 contain the same elements, false otherwise */
  def listContainsSameElements(l1: List[(Array[Double], Array[Double], Array[Double])], l2: List[(Array[Double], Array[Double], Array[Double])]): Boolean = {
    for (i <- 0 until l2.length) {
      if (!inList(l2(i), l1))
        return false
    }
    true
  }
  
  /** Returns a List (l1 U l2) containing only non-dominated points.
    *
    * l1 is already formed by non-dominated points.
    * 
    * @param l1 A list of triple (point, evaluations, step parameter) containing
    *        only non-dominated points
    * @param l2 A list of triple (point, evaluations, step parameter) 
    * @return A list of triple (point, evaluations, step parameter) containing the elements
    *         of l1 and l2 from which the dominated points have been removed */
  def filter(l1: List[(Array[Double], Array[Double], Array[Double])], l2In: List[(Array[Double], Array[Double], Array[Double])]): List[(Array[Double], Array[Double], Array[Double])] = {
    //Initializing l3 as the union of l1 and l2
    var l2 = l2In
    var l3 = List[(Array[Double], Array[Double], Array[Double])]()
    for (i <- 0 until l1.length) {
      if (!inList(l1(i), l3))
        l3 = l3 ::: List(l1(i))
    }
    for (i <- 0 until l2.length) {
      if (!inList(l2(i), l3))
        l3 = l3 ::: List(l2(i))
    }
    val a2 = l2.toArray
    val a3 = l3.toArray
    //Removing dominated points
    var i = 0
    for (i <- 0 until a2.length) {
      var j = 0
      var stop = false
      while (j < a3.length && !stop) {
        if (!areEqual(a2(i)._2, a3(j)._2) && dominates(a3(j), a2(i))) {
          l3 = remove(a2(i), l3)
          stop = true
        }
        j += 1
      }
      if (!stop) {
        for (j <- 0 until a3.length) {
          if (!areEqual(a2(i)._2, a3(j)._2) && dominates(a2(i), a3(j))) {
            val pToRemove = a3(j)
            l3 = remove(pToRemove, l3)
            if (inList(pToRemove, l2)) {
              l2 = remove(pToRemove, l2)
            }
          }
        }
      }
    }
    l3
  }
  
  /** Returns a triple (Point, Evaluations, Step parameter) representing the new point created from the basis and the point.
    *
    * The new point is generated according to the following formula:
    * new point = former point + alpha * basis
    * 
    * @param iterate A triple (point, evaluations, step parameter) from which the new point will be created
    * @param d An array of Double representing the base that will be used to create the new point.
    * @return A triple (point, evaluations, step parameter) being the new point created from the specified input,
    *         the evaluations of the objective functions at this point and the step parameter of the input
    *         multiplied by the increase step factor */
  def createNewPoint(iterate: (Array[Double], Array[Double], Array[Double]), d: Array[Double], tol: Double): (Array[Double], Array[Double], Array[Double]) = {
    val newPoint = Array.tabulate(iterate._1.length)(i => iterate._1(i) + (iterate._3(i) * d(i)))
    //If the new iterate is not in the domain, we set its objectives to +inf and we don't evaluate it
    if (!inDomain(newPoint))
      return (newPoint, Array.fill(nbObjectives){Double.PositiveInfinity}, Array.tabulate(iterate._3.length)(i => gamma * iterate._3(i)))
    val evals = evaluator.eval(objFunctions,newPoint)
    evalCount += evals._2
    (newPoint, evals._1, Array.tabulate(iterate._3.length)(i => gamma * iterate._3(i)))
  }
  
  /** Initializes the archive with the specified number of points generated from the specified method.
    * 
    * @param method A string representing the method used to generate points:
    *        1, nLine, nRand or nQuasiRand
    * @param alphaInit An array of Double being the initial step parameter 
    * @param betaInit A Double that will initialize the value of the decrease step parameter
    * @param gammaInit A Double that will initialize the value of the increase step parameter
    * @param basesInit An array of arrays of double being the set of initial bases to use for the poll step */
  def initialization(method: String, alphaInit: Array[Double], betaInit: Double, gammaInit: Double, basesInit: Array[Array[Double]]) = {
    //Reseting the evaluation counter
    evalCount = 0
    //Initializing increase and decrease step parameters
    gamma = gammaInit
    beta = betaInit
    //Initializing the bases d
    for (i <- 0 until basesInit.length)
      d = d ::: List(basesInit(i))
    for (i <- 0 until basesInit.length)
      d = d ::: List(negateVector(basesInit(i)))
    //Initializing the archive
    val n = 50
    method match {
      //We take a single point in the middle of the domain as initial archive
      case "1" => {
        val newPoint = Array.tabulate(dom.length)(i => dom(i).min + (dom(i).size / 2))
        lk = lk ::: List((newPoint, objFunctions(newPoint), alphaInit))
        evalCount += 1
      }
      //We take n points equally spaced on the line connecting l and u (being the lower and the upper bounds of the domain) as initial archive
      case "nLine" => {
        val newPoints = Array.fill(n){Array.fill(dom.length){0.0}}
        for (i <- 0 until n)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (n - 1).toDouble) * dom(j).size))
        for (i <- 0 until n) {
          val evals = objFunctions(newPoints(i))
          lk = lk ::: List((newPoints(i), evals, alphaInit))
        }
        evalCount += n
      }
      //We take n points uniformly randomly generated on the domain as initial archive
      case "nRand" => {
        val newPoints = Array.fill(n){Array.fill(dom.length){0.0}}
        for (i <- 0 until n)
          newPoints(i) = Array.tabulate(dom.length)(i => dom(i).min + (rand.nextDouble * dom(i).size))
        for (i <- 0 until n)
          lk = lk ::: List((newPoints(i), objFunctions(newPoints(i)), alphaInit))
        evalCount += n
      }
      //We take n points generated with a quasi-random sequence on the domain as initial archive
      case "nQuasiRand" => {
        val newPoints = sampler.scrambledHaltonSequence(n, dom)
        for (i <- 0 until n) {
          lk = lk ::: List((newPoints(i), objFunctions(newPoints(i)), alphaInit))
        }
        evalCount += n
      }
      //Default case: see "nLine"
      case _ => {
        val newPoints = Array.fill(n){Array.fill(dom.length){0.0}}
        for (i <- 0 until n)
          newPoints(i) = Array.tabulate(dom.length)(j => dom(j).min + (( i.toDouble / (n - 1).toDouble) * dom(j).size))
        for (i <- 0 until n)
          lk = lk ::: List((newPoints(i), objFunctions(newPoints(i)), alphaInit))
        evalCount += n
      }
    }
    //Filters the initial archive so that it contains only non-dominated points
    lk = filter(List[(Array[Double], Array[Double], Array[Double])](), lk)
  }
  
  /** Selection of a point in the archive that will be the next iterate.
    *
    * The next iterate is the point for which there is the biggest gap between
    * itself and its direct neighbor.
    * 
    * @param iteration An Int representing the number of the current iteration 
    * @return A triple (point, evaluations, step parameter) that will be the iterate
    *         for the current iteration */
  def selectIterate(iteration: Int): (Array[Double], Array[Double], Array[Double]) = {
    if(failedIter >= 5) {
      lk = lk.drop(1) ::: lk.take(1)
      return lk(0)
    }
    val sortedLk = sortLk
    var archlk = List[(Array[Double], Array[Double])]()
    for (i <- 0 until lk.length)
      archlk = archlk ::: List((lk(i)._1, lk(i)._2))
    val scaleObj = scaleObjectives(archlk)
    var maxDist = Double.MinValue
    var iterate = sortedLk(0)
    for (i <- 0 until sortedLk.length - 1) {
      val curDist = computeScaledDistance(sortedLk(i)._2, sortedLk(i + 1)._2, scaleObj)
      if (maxDist < curDist) {
        maxDist = curDist
        iterate = if (iteration %2 == 0) sortedLk(i) else sortedLk(i + 1)
      }
      //When there is a tie break, the point with the biggest step size is chosen
      else if (maxDist == curDist) {
        if (average(sortedLk(i)._3) > average(iterate._3))
          iterate = sortedLk(i)
      }
    }
    iterate
  }
  
  /** The optional search step that tries some new points around points in the current lk and updates lk in consequence. */
  def searchStep(iterate: (Array[Double], Array[Double], Array[Double])) = {
    if (lk.length > 1) {
      val randIndex = rand.nextInt(lk.length)
      val randPoint = lk(randIndex)
      if (!areEqual(iterate._2, randPoint._2)) {
        val randDistance = 0.25 + (rand.nextDouble * 0.5)
        val newPoint = Array.tabulate(iterate._1.length)(i => iterate._1(i) + (randDistance * (randPoint._1(i) - iterate._1(i))))
        if (inDomain(newPoint)) {
          val evals = evaluator.eval(objFunctions,newPoint)
          evalCount += evals._2
          val newCompletePoint = (newPoint, evals._1, Array.tabulate(iterate._3.length)(i => dom(i).size/10.0))
          val lFiltered = filter(lk, List(newCompletePoint))
          if (!listContainsSameElements(lk, lFiltered)) {
            lk = lFiltered
            failedIter = 0
          }
        }
      }
    }
  }
  
  /** The poll step that tries some new points in specified directions around current iterate and updates lk in consequence. Returns true if iteration was successful, false otherwise.
    *
    * @param iterate A triple (point, evaluations, step parameter) around which we will evaluate new points
    * @return A Boolean that is true if the poll step was successful (archive has changed), false otherwise */
  def pollStep(iterate: (Array[Double], Array[Double], Array[Double]), tol: Double): Boolean = {
    //Creating the list of the new points
    var lAdd = List[(Array[Double], Array[Double], Array[Double])]()
    for (i <- 0 until d.length) {
      lAdd = lAdd ::: List(createNewPoint(iterate, d(i), tol))
    }
    //Updating lk wrt the new points created
    var lFiltered = filter(lk, lAdd)
    if(listContainsSameElements(lk, lFiltered))
      return false
    lk = lFiltered
    true
  }

  /** The poll step that tries some new points in specified directions around current iterate and updates lk in consequence. Returns true if iteration was successful, false otherwise.
    *
    * This poll step is opportunistic (i.e. as soon as the archive has changed, it ends).
    *
    * @param iterate A triple (point, evaluations, step parameter) around which we will evaluate new points
    * @return A Boolean that is true if the poll step was successful (archive has changed), false otherwise */
  def opportunisticPollStep(iterate: (Array[Double], Array[Double], Array[Double]), tol: Double): Boolean = {
    for (i <- 0 until d.length) {
      val np = createNewPoint(iterate, d(i), tol)
      //Updating lk wrt the new point created
      val lFiltered = filter(lk, List(np))
      if (!listContainsSameElements(lk, lFiltered)) {
        lk = lFiltered
        return true
      }
    }
    false
  }
  
  /** Updates the step size parameter of the current iterate.
    * 
    * The step size parameter increases if the iteration was successful and decreases otherwise.
    * 
    * @param iterate A triple (point, evaluations, step parameter) that is the iterate for which we
    *        want to modify the step parameter
    * @param successfulIteration A Boolean that is true if the iteration was successful, false otherwise */
  def stepSizeParameterUpdate(iterate: (Array[Double], Array[Double], Array[Double]), successfulIteration: Boolean) = {
    for (i <- 0 until lk.length) {
      if (areEqual(iterate._1, lk(i)._1)) {
        val newAlpha = Array.tabulate(iterate._3.length)(i => if(successfulIteration) gamma * iterate._3(i) else beta * iterate._3(i))
        lk = lk.take(i) ::: List((lk(i)._1, lk(i)._2, newAlpha)) ::: lk.drop(i + 1)
      }
    }
  }
  
  /** One iteration of the DMS algorithm.
    * 
    * @param iteration An Int that is the number of the current iteration
    * @param tol The tolerance which defines the desired level of precision */
  def iterDMS(iteration: Int , tol: Double) = {
    //Selection of the iterate
    val iterate = selectIterate(iteration)
    //Optional search step
    searchStep(iterate)
    //Poll step
    val successfulIteration = opportunisticPollStep(iterate, tol)
    if(successfulIteration)
      failedIter = 0
    else
      failedIter += 1
    //Step size parameter update
    stepSizeParameterUpdate(iterate, successfulIteration)
  }
  
  override def findPareto(nbPoints: Int, tol: Double): List[(Array[Double], Array[Double])] = {
    var iterCount = 0
    initialization("nLine", Array.tabulate(dom.length)(i => dom(i).size/20.0), 0.5, 2.0, proposedBases)
    while (lk.length < 100 && evalCount < 5000) {
      iterDMS(iterCount, tol)
      iterCount += 1
    }
    var pareto = List[(Array[Double], Array[Double])]()
    for (i <- 0 until lk.length) {
      pareto = pareto ::: List((lk(i)._1, lk(i)._2))
    }
    pareto
  }
}

/** Factory for MODMS instances. */
object MODMS {
  /** Create a MODMS according to the specified arguments.
    * 
    * @param objFunctions The objective functions for which we want to find the Pareto set
    * @param objBounds The maximal values for the objectives
    * @param dom The domain of the search space
    * @param proposedBases The bases used for the poll step */
  def apply (objFunctions: Array[Double] => Array[Double], objBounds: Array[Double], dom: Array[Interval],
      proposedBases: Array[Array[Double]]) = new MODMS(objFunctions, objBounds, dom, proposedBases)
}
