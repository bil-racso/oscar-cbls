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
package oscar.dfo.multiobjective.mogen.algos.states

import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.utils._

class DirectionalDirectSearchState[E <% Ordered[E]](initPoint: MOOPoint[E], val stepSizes: Array[Double], val dictionary: Array[Array[Double]], var basisSize: Int) extends ComparativeAlgorithmState[E] {
  val newDirectionProportion = 0.2
  var bestPoint = initPoint
  var currentBasis = getRandomBasis
  
  def getBestPoint = bestPoint
  
  def getPoints = List(bestPoint)
  
  def getNewState(newBestPoint: MOOPoint[E], comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    DirectionalDirectSearchState(newBestPoint, stepSizes, dictionary, basisSize)
  }
  
  def promoteDirection(directionIndex: Int) {
    currentBasis = currentBasis(directionIndex) :: currentBasis.take(directionIndex) ::: currentBasis.drop(directionIndex + 1)
  }
  
  def getNewPoint(directionIndex: Int, evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): MOOPoint[E] = {
    val newCoordinates = Array.tabulate(bestPoint.nbCoordinates)(i => bestPoint.coordinates(i) + dictionary(currentBasis(directionIndex))(i))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def updateBasis = {
    currentBasis = currentBasis.dropRight((newDirectionProportion * basisSize).toInt)
    var potentialDirections = List((0 until dictionary.size): _*).filter(index => !currentBasis.contains(index))
    for (i <- 1 to (basisSize - currentBasis.size)) {
      currentBasis ::= getNewRandomDirection(currentBasis)
    }
  }
  
  def getRandomBasis: List[Int] = {
    var randomBasis = List[Int]()
    var potentialDirections = List((0 until dictionary.size): _*)
    for (i <- 1 to basisSize) {
      randomBasis ::= getNewRandomDirection(randomBasis)
    }
    randomBasis
  }
  
  def getNewRandomDirection(basis: List[Int]): Int = {
    List((0 until dictionary.size): _*).filter(index => !basis.contains(index))(RandomGenerator.nextInt(dictionary.length - basis.length))
  }
  
  def increaseStepSizes = stepSizes.foreach(_ * DirectionalDirectSearchState.increaseFactor)
  def decreaseStepSizes = stepSizes.foreach(_ * DirectionalDirectSearchState.decreaseFactor)
}

object DirectionalDirectSearchState {
  var increaseFactor = 1.1
  var decreaseFactor = 0.9
  
  def apply[E <% Ordered[E]](initPoint: MOOPoint[E], stepSizes: Array[Double], dictionnary: Array[Array[Double]], basisSize: Int) = new DirectionalDirectSearchState(initPoint, stepSizes, dictionnary, basisSize)
  
  def apply[E <% Ordered[E]](initPoint: MOOPoint[E], stepSizeIntervals: Array[(Double, Double)], dictionarySize: Int = 100, basisSize: Int = 10): ComparativeAlgorithmState[E] = {
    val stepSizes = Array.tabulate(initPoint.nbCoordinates)(i => stepSizeIntervals(i)._1 + RandomGenerator.nextDouble * (stepSizeIntervals(i)._2 - stepSizeIntervals(i)._1))
    val dictionary = Array.tabulate(dictionarySize)(i => normalizeArray(Array.tabulate(initPoint.nbCoordinates)(i => RandomGenerator.nextDouble)))
    DirectionalDirectSearchState(initPoint, stepSizes, dictionary, basisSize)
  }
  
  def normalizeArray(coordinates: Array[Double]): Array[Double] = {
    val vectorLength = math.sqrt(coordinates.foldLeft(0.0)((acc, newDim) => acc + newDim * newDim))
    Array.tabulate(coordinates.length)(i => coordinates(i) / vectorLength)
  }
}
