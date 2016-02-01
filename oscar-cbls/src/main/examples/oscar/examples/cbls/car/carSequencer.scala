package oscar.examples.cbls.car

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.objective.Objective
import oscar.cbls.search.combinators.{Profile, DynAndThen}
import oscar.cbls.search.move.SwapMove

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap
import scala.util.Random

/**
 * Created by rdl on 29-01-16.
 */
object carSequencer  extends CBLSModel with App {

  val orderedCarsByType:SortedMap[Int,Int] = SortedMap(0 -> 13, 1 -> 6, 2 -> 10 , 3 -> 10, 4 -> 4, 5 -> 6)

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
  c.post(sequence(carSequence,3,2,airCoCarTypes.contains(_)))

  //automaticGearBox: max 3 out of 5
  c.post(sequence(carSequence,5,3,automaticGearBoxCarTypes.contains(_)))

  //diesel: max 3 out of 5
  c.post(sequence(carSequence,5,3,dieselCarTypes.contains(_)))

  //esp: max 2 ouf of 3
  c.post(sequence(carSequence,3,2,espCarTypes.contains(_)))

  println("closing model")

  c.close
  val obj:Objective = c.violation

  s.close()

  val swap = swapsNeighborhood(carSequence,"swapCars")

  val linkedDoubleSwaps = DynAndThen(
    swapsNeighborhood(carSequence,"swapCars1"),
    ((swapMove:SwapMove) => {
      val indices = List(swapMove.idI, swapMove.idJ)
      swapsNeighborhood(carSequence, "swapCars2", searchZone1 = () => indices, symmetryCanBeBrokenOnIndices = false, symmetryCanBeBrokenOnValue = true)
    })) name "linkedDoubleSwaps"

  val doubleSwaps = (swapsNeighborhood(carSequence,"swapCars1") andThen swapsNeighborhood(carSequence,"swapCars2")) name "doubleSwaps"

  val search = Profile(
    Profile(swap)
      orElse Profile(linkedDoubleSwaps) //orElse doubleSwaps
      orElse (randomSwapNeighborhood(carSequence,nbCars/5,"smallRandomize") maxMoves (nbCars / 5))
      orElse (randomSwapNeighborhood(carSequence,nbCars/2,"bigRandomize") maxMoves (nbCars / 5))
      saveBestAndRestoreOnExhaust obj)

  search.verbose = 1

  search.doAllMoves(_ => c.isTrue,obj)

  println(search.profilingStatistics)
  println(c.violation)
  println("car sequence:" + carSequence.map(_.value).mkString(","))

}
