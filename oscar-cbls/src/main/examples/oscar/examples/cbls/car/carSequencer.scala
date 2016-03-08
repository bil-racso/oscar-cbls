package oscar.examples.cbls.car

import java.awt.Dimension

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.objective.Objective
import oscar.cbls.search.RollNeighborhood
import oscar.cbls.search.combinators.{Profile, DynAndThen}
import oscar.cbls.search.move.{Move, SwapMove}
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.FunctionGraphic.{Zoom, ObjFunctionGraphicContainer}
import oscar.visual.VisualFrame

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap
import scala.util.Random
import scala.language.postfixOps
/**
 * Created by rdl on 29-01-16.
 */
object carSequencer  extends CBLSModel with App {

  val orderedCarsByType:SortedMap[Int,Int] = SortedMap(0 -> 110, 1 -> 60, 2 -> 110, 3 -> 120, 4 -> 40, 5 -> 30)

  println("carSequencing")
  println("orderedCarTypes:" + orderedCarsByType)

  //option types
  //   A   G   D   E
  //0  T   T   T   F
  //1  F   T   T   F
  //2  T   F   T   F
  //3  F   F   F   T
  //4  T   T   F   T
  //5  F   T   F   T

  val airCoCarTypes = SortedSet(0,2,4)
  val automaticGearBoxCarTypes = SortedSet(0,1,4,5)
  val dieselCarTypes = SortedSet(0,1,2)
  val espCarTypes = SortedSet(3,4,5)

  val maxType = orderedCarsByType.keys.max
  val minType = orderedCarsByType.keys.min
  val typeRange = minType to maxType
  def prependItems(acc:List[Int],n:Int,item:Int):List[Int] = (if(n == 0) acc else prependItems(item :: acc,n-1,item))
  val orderedCarTypes:List[Int] = orderedCarsByType.foldLeft(List.empty[Int])({case (accList,(carType,nbItems)) => prependItems(accList,nbItems,carType)})
  val nbCars = orderedCarTypes.size

  println("totalNumberOfCars:" + nbCars)

  //initializes the car sequence in a random way
  val orderedCarTypesIterator = Random.shuffle(orderedCarTypes).toIterator
  val carSequence:Array[CBLSIntVar] = Array.tabulate(nbCars)(p => CBLSIntVar(orderedCarTypesIterator.next(),typeRange,"carClassAtPosition" + p)).toArray

  //airConditionner: max 2 out of 3
  c.post(sequence(carSequence,3,2,airCoCarTypes.contains))

  //automaticGearBox: max 3 out of 5
  c.post(sequence(carSequence,5,3,automaticGearBoxCarTypes.contains))

  //diesel: max 3 out of 5
  c.post(sequence(carSequence,5,3,dieselCarTypes.contains))

  //esp: max 2 ouf of 3
  c.post(sequence(carSequence,3,2,espCarTypes.contains))

  val impactZone = 5

  val varViolation = c.violations(carSequence)
  val violatedCars = filter(varViolation)
  val mostViolatedCars = argMax(varViolation)

  println("closing model")

  c.close
  val obj:Objective = c.violation

  s.close()

  startWatch()

  val swap = swapsNeighborhood(carSequence,"swapCars")

  val roll = RollNeighborhood(carSequence, name = "RollCars", maxShiftSize = _ => 10)
  val mostViolatedSwap = swapsNeighborhood(carSequence,"mostViolatedSwap", searchZone2 = mostViolatedCars, symmetryCanBeBrokenOnIndices = false)
  val shiftNeighbor = shiftNeighborhood(carSequence, searchZone1 =() => violatedCars.value.toList/*, maxShiftSize = carSequence.length/2, maxOffsetSize = carSequence.length/2*/, hotRestart = true)
  val rollNeighbor = rollNeighborhood(carSequence)

  val linkedDoubleSwaps = DynAndThen(
    swapsNeighborhood(carSequence,"swapCars1"),
    ((swapMove:SwapMove) => {
      val indices = List(swapMove.idI, swapMove.idJ)
      swapsNeighborhood(carSequence, "swapCars2", searchZone1 = () => indices, symmetryCanBeBrokenOnIndices = false, symmetryCanBeBrokenOnValue = true)
    })) name "linkedDoubleSwaps"

  val doubleSwaps = (swapsNeighborhood(carSequence,"swapCars1") andThen swapsNeighborhood(carSequence,"swapCars2")) name "doubleSwaps"

  val looselyLinkedDoubleSwaps = DynAndThen(
    swapsNeighborhood(carSequence,"swapCars1", symmetryCanBeBrokenOnIndices = false),
    ((swapMove:SwapMove) => {
      val firstSwappedCar = 0.max(swapMove.idI - impactZone) until (nbCars).min(swapMove.idI + impactZone)
      val otherSwappedCar = (nbCars-1).min(swapMove.idJ+1) until nbCars
      swapsNeighborhood(carSequence, "swapCars2", searchZone1 = () => firstSwappedCar, searchZone2 = () => otherSwappedCar, symmetryCanBeBrokenOnIndices = false)
    })) name "looselyLinkedDoubleSwaps"

  val search2 = (
    Profile(mostViolatedSwap) orElse Profile(roll)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, mostViolatedCars, name = "shuffleMostViolatedCars")), 5, obj)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, violatedCars, name = "shuffleSomeViolatedCars", numberOfShuffledPositions = () => violatedCars.value.size/2)), 2, obj)
      exhaust Profile(shiftNeighbor)showObjectiveFunction(obj)
    )

  val search = Profile(
    Profile(mostViolatedSwap random swap)
      orElse (Profile(shiftNeighbor))
      orElse (shuffleNeighborhood(carSequence, mostViolatedCars, name = "shuffleMostViolatedCars") maxMoves (10))
      orElse (shuffleNeighborhood(carSequence, violatedCars, name = "shuffleAllViolatedCars") maxMoves (10))
      orElse (shuffleNeighborhood(carSequence, name = "globalShuffle") maxMoves (10))
      maxMoves nbCars *2 withoutImprovementOver obj
      saveBestAndRestoreOnExhaust obj) afterMove(checkGrouped)
  //.afterMove({println("most violated positions: " + mostViolatedCars.value + " car types: " + mostViolatedCars.value.toList.map(carSequence(_).value))})

  search2.verbose = 1
  search2.paddingLength = 150
  search2.doAllMoves(_ => c.isTrue,obj)

  println(search2.profilingStatistics)

  /*search.verbose = 1
  search.paddingLength = 150
  search.doAllMoves(_ => c.isTrue,obj)

  println(search.profilingStatistics)*/

  println(c.violation)
  println("car sequence:" + carSequence.map(_.value).mkString(","))

  println("grouped:" + carSequence.map(_.value).toList.groupBy[Int]((c:Int) => c).mapValues((l:List[Int]) => l.size))

  def checkGrouped(): Unit ={
    val groupedValuesInSlution = carSequence.map(_.value).toList.groupBy[Int]((c:Int) => c).mapValues((l:List[Int]) => l.size)
    for((carType,count) <- groupedValuesInSlution){
      require(orderedCarsByType(carType) == count, "car count changed on car type " + carType)
    }
  }
}
