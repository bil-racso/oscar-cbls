package oscar.dfo.mogen.algos

import scala.reflect.ClassTag
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.util.mo.RandomGenerator
import oscar.util.mo.LinearList

object NelderMead extends ComparativeAlgorithm {
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case nmState: NelderMeadState[E] => {
        val centroid = nmState.getCentroid
        val reflectedPoint = nmState.getReflection(evaluator, feasReg, centroid)
        //println("=" * 30 + " " * 5 + "SIMPLEX" + " " * 5 + "=" * 30)
        //for (point <- nmState.simplex) println(point)
        //println("=" * 30 + " " * 5 + "REFLEXION" + " " * 5 + "=" * 30)
        //println(reflectedPoint)
        //println("=" * 80)
        // The reflected point is better than the second worst point of the simplex but worse or equivalent to the best point (f^0 <= f^r < f^(n-1) for SO minimisation problems)
        if (comparator.cmpWithArchive(nmState.bestPoint, reflectedPoint, currentArchive) && comparator.cmpWithArchive(reflectedPoint, nmState.simplex(nmState.simplexSize - 2), currentArchive) &&
            !currentArchive.contains(reflectedPoint) && feasReg.isFeasible(reflectedPoint.coordinates)) {
          nmState.applySinglePointTransformation(reflectedPoint, comparator)
          println("Relected Point: " + reflectedPoint)
          return List(reflectedPoint)
        }
        else {
          // The reflected point was better than the best point of the simplex => Expansion performed
          if (comparator.cmpWithArchive(reflectedPoint, nmState.bestPoint, currentArchive)) {
            val expandedPoint = nmState.getExpansion(evaluator, feasReg, centroid)
            //println("reflectedPoint: " + reflectedPoint)
            //println("bestPoint: " + nmState.bestPoint)
            //println("expandedPoint: " + expandedPoint)
            // The expanded point is better than the reflected point
            if (comparator.cmpWithArchive(expandedPoint, reflectedPoint, currentArchive)) {
              nmState.applySinglePointTransformation(expandedPoint, comparator)
              println("Expanded Point better than reflected Point")
              return List(expandedPoint)
            }
            // The reflected point is better than the expanded point
            else {
              println("Expanded Point worse than relected Point")
              nmState.applySinglePointTransformation(reflectedPoint, comparator)
              return List(reflectedPoint)
            }
          }
          // The reflected point was worse than the second worse point of the vertex (f^(n-1) <= f^r for SO minimisation problems)
          else {
            val contractedPoint =
            // The reflected point is better than the worst point of the simplex => Outside contraction
            if (comparator.cmpWithArchive(reflectedPoint, nmState.worstPoint, currentArchive)) nmState.getOutsideContraction(evaluator, feasReg, centroid)
            // The reflected point is worse than the worst point of the simplex => Inside contraction
            else nmState.getInsideContraction(evaluator, feasReg, centroid)
            // The contracted point is better than the reflected point
            if (comparator.cmpWithArchive(contractedPoint, reflectedPoint, currentArchive)) {
              println("Contraction")
              nmState.applySinglePointTransformation(contractedPoint, comparator)
              return List(contractedPoint)
            }
            // The contracted point is worse than the reflected point => shrink
            else {
              println("Shrink")
              nmState.applyShrink(comparator, evaluator, feasReg)
              return nmState.simplex.toList
            }
          }
        }
      }
      case _ => throw new IllegalArgumentException("The Nelder-Mead algorithm can only be used with a state for Nelder-Mead");
    }
  }
  
  
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    NelderMeadState(coordinates, startIntervals, evaluator, feasReg, comparator)
  }
}

class NelderMeadState[E <% Ordered[E]](val simplex: Array[MOOPoint[E]]) extends ComparativeAlgorithmState[E] {
  
  val simplexSize = simplex.length
  var bestPoint = simplex(0)
  var deltaR = 1
  var deltaE = 2
  var deltaOC = 0.5
  var deltaIC = -0.5
  var gammaS = 0.5
  
  def getBestPoint = bestPoint
  
  def nbCoordinates = simplex(0).nbCoordinates
  
  def worstPoint = simplex(simplexSize - 1)
  
  def getNewState(newBestPoint: MOOPoint[E], comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    val newState = NelderMeadState(simplex)
    newState.deltaR = this.deltaR
    newState.deltaE = this.deltaE
    newState.deltaOC = this.deltaOC
    newState.deltaIC = this.deltaIC
    newState.gammaS = this.gammaS
    newState.bestPoint = newBestPoint
    orderSimplex(comparator)
    newState
  }
  
  def orderSimplex(comparator: MOOComparator[E]) = {
    simplex.sortWith((point1, point2) => ((point1 == bestPoint) || (comparator.isEquivalent(point1, point2) && 0.5 > RandomGenerator.nextDouble) || comparator.dominates(point1, point2)))
  }
  
  def getReflection(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaR, evaluator, feasibleReg)
  
  def getExpansion(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaE, evaluator, feasibleReg)
  
  def getInsideContraction(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaIC, evaluator, feasibleReg)
  
  def getOutsideContraction(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaOC, evaluator, feasibleReg)
  
  def getSinglePointTransformation(centroid: Array[Double], factor: Double, evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): MOOPoint[E] = {
    val newCoordinates = NelderMeadState.arraySum(centroid, NelderMeadState.arrayProd(NelderMeadState.arrayDiff(centroid, worstPoint.coordinates), factor))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def applySinglePointTransformation(newPoint: MOOPoint[E], comparator: MOOComparator[E]) = {
    simplex(simplexSize - 1) = newPoint
    orderSimplex(comparator)
  }
  
  def getCentroid: Array[Double] = {
    val allButWorstCoordinates = simplex.map(mooP => mooP.coordinates).take(simplexSize - 1)
    NelderMeadState.arrayProd(allButWorstCoordinates.drop(1).foldLeft(allButWorstCoordinates(0))((acc, newCoords) => NelderMeadState.arraySum(acc, newCoords)), 1.0 / (simplexSize - 1))
  }
  
  def applyShrink(comparator: MOOComparator[E], evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion) = {
    val simplexCoordinates = simplex.map(mooP => mooP.coordinates).drop(1)
    for (i <- 1 until simplexSize - 1) {
      simplex(i) = evaluator.eval(NelderMeadState.arrayProd(simplexCoordinates(i), gammaS), feasibleReg)
    }
    orderSimplex(comparator)
  }
  
  def printSimplex = {
    println("=" * 80)
    for (i <- 0 until simplexSize)
      println(i + ": " + simplex(i).toString)
  }
  
  def getPoints: List[MOOPoint[E]] = simplex.toList
}

object NelderMeadState {
  def apply[E <% Ordered[E]](simplex: Array[MOOPoint[E]]) = new NelderMeadState(simplex)
  
  def apply[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    val simplex = Array.tabulate(coordinates.length + 1){ index =>
      if (index == 0) coordinates
      else NelderMeadState.arraySum(coordinates, startIntervals.map(e => (0.5 - RandomGenerator.nextDouble) * math.abs(e._2 - e._1) * 0.05))
    }
    NelderMeadState(simplex.map(coord => evaluator.eval(coord, feasReg)))
  }
  
  def arraySum(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) + ar2(i))
  
  def arrayDiff(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) - ar2(i))
  
  def arrayProd(ar: Array[Double], factor: Double): Array[Double] = Array.tabulate(ar.length)(i => factor * ar(i))
}