package oscar.dfo.mogen.algos.states

import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOEvaluator
import oscar.util.mo.RandomGenerator
import oscar.util.mo.FeasibleRegion

class DirectionalDirectSearchState[E <% Ordered[E]](initPoint: MOOPoint[E], var stepSizes: Array[Double], val dictionary: Array[Array[Double]], var basisSize: Int) extends ComparativeAlgorithmState[E] {
  val originalStepSizes = stepSizes.clone
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
    val newCoordinates = Array.tabulate(bestPoint.nbCoordinates)(i => bestPoint.coordinates(i) + stepSizes(i) * dictionary(currentBasis(directionIndex))(i))
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
  
  def reInitializeStepSizes = stepSizes = originalStepSizes.clone
  
  def getNewRandomDirection(basis: List[Int]): Int = {
    List((0 until dictionary.size): _*).filter(index => !basis.contains(index))(RandomGenerator.nextInt(dictionary.length - basis.length))
  }
  
  def increaseStepSizes = stepSizes = stepSizes.map((dim: Double) => dim * DirectionalDirectSearchState.increaseFactor)
  def decreaseStepSizes = stepSizes = stepSizes.map((dim: Double) => dim * (DirectionalDirectSearchState.decreaseFactorLowerBound + RandomGenerator.nextDouble * (DirectionalDirectSearchState.decreaseFactorUpperBound - DirectionalDirectSearchState.decreaseFactorLowerBound)))
}

object DirectionalDirectSearchState {
  var increaseFactor = 1.25
  var decreaseFactorUpperBound = 0.9
  var decreaseFactorLowerBound = 0.5
  
  def apply[E <% Ordered[E]](initPoint: MOOPoint[E], stepSizes: Array[Double], dictionnary: Array[Array[Double]], basisSize: Int) = new DirectionalDirectSearchState(initPoint, stepSizes, dictionnary, basisSize)
  
  def apply[E <% Ordered[E]](initPoint: MOOPoint[E], stepSizeIntervals: Array[(Double, Double)], dictionarySize: Int = 100, basisSize: Int = 10): ComparativeAlgorithmState[E] = {
    val stepSizes = Array.tabulate(initPoint.nbCoordinates)(i => stepSizeIntervals(i)._1 + RandomGenerator.nextDouble * (stepSizeIntervals(i)._2 - stepSizeIntervals(i)._1))
    val positiveDictionary = Array.tabulate(dictionarySize)(i => normalizeArray(Array.tabulate(initPoint.nbCoordinates)(i => RandomGenerator.nextDouble)))
    val negativeDictionary = Array.tabulate(dictionarySize)(i => Array.tabulate(initPoint.nbCoordinates)(j => - positiveDictionary(i)(j)))
    val dictionary = RandomGenerator.shuffle(Array.tabulate(2 * dictionarySize)(i => if(i < dictionarySize) positiveDictionary(i) else negativeDictionary(i - dictionarySize)).toSeq).toArray
    DirectionalDirectSearchState(initPoint, stepSizes, dictionary, basisSize)
  }
  
  def normalizeArray(coordinates: Array[Double]): Array[Double] = {
    val vectorLength = math.sqrt(coordinates.foldLeft(0.0)((acc, newDim) => acc + newDim * newDim))
    Array.tabulate(coordinates.length)(i => coordinates(i) / vectorLength)
  }
}