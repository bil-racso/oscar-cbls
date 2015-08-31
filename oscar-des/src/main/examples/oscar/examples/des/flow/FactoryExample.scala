package oscar.examples.des

import oscar.des.engine.Model
import oscar.des.flow.modeling.FactorySimulationHelper
import scala.language.implicitConversions

/**
 * Created by rdl on 2/02/2015.
 */

object FactoryExample extends App with FactorySimulationHelper {

  val starttime = System.currentTimeMillis

  def probability(p:Double)():Boolean = {
    math.random < p
  }
  def choose(r:Range)():Int = {
    r.apply ((math.random * r.size).toInt)
  }

  val m = new Model
  val verbose = false

  val standardItemClass = zeroItemClass
  val choseZeroOne = outputValue(choose(0 to 1))

  //time unit is the second
  val rawMaterialStorage = fIFOStorage(200,List((40,standardItemClass)),"rawMaterialStorage", verbose,false)


  val rawSupplier = singleBatchProcess(m, 5000, Array(), Array((()=>1,rawMaterialStorage)), identity, "rawSupplier", verbose)
  val ordering = onLowerThreshold(rawMaterialStorage,
    m,
    rawSupplier,
    20,
    (size:Int) => 20,
    true,
    0,
    "orderingPolicy")

  val inputFeederOfDieCuttingPartA = fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA", verbose,false)

  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = singleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), identity, "transportingToDieCutterA", verbose)

  val outputSlotOfDieCuttingPArtA = lIFOStorage(400,Nil,"outputSlotOfDieCuttingPArtA", verbose,false)
  val dieCuttingPartA = singleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), identity, "dieCuttingPartA", verbose)

  val bufferForDieA =  fIFOStorage(500,Nil,"bufferForDieA", verbose,false)
  val outputBeltFromDieCuttingA = conveyorBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtA)), List((1, bufferForDieA)), identity, "outputBeltFromDieCuttingA", verbose)


  val inputAOfForming = fIFOStorage(2, Nil, "inputAOfForming", verbose, false)
  val transportingFromBufferA = conveyorBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieA)), List((1, inputAOfForming)), identity, "transportingFromBufferA", verbose)

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = fIFOStorage(100, Nil, "inputFeederOfDieCuttingPartB", verbose, false)

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = singleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartB)), identity, "transportingToDieCutterB", verbose)

  val outputSlotOfDieCuttingPArtB = lIFOStorage(4, Nil, "outputSlotOfDieCuttingPArtB", verbose, true)
  val dieCuttingPartB = singleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartB)), Array((()=>4,outputSlotOfDieCuttingPArtB)), identity, "dieCuttingPartB", verbose)

  val bufferForDieB = lIFOStorage(500, Nil, "outputSlotOfDieCuttingPArtA", verbose, false)
  val outputBeltFromDieCuttingB = conveyorBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtB)), List((1, bufferForDieB)), identity, "outputBeltFromDieCuttingB", verbose)

  val inputBOfForming = lIFOStorage(2, Nil, "inputBOfForming", verbose, false)
  val transportingFromBufferB = conveyorBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieB)), List((1, inputBOfForming)), identity, "transportingFromBufferB", verbose)

  val outputContainerOfForming = lIFOStorage(120, Nil, "outputContainerOfForming", verbose,false)
  val forming = singleBatchProcess(m, 30, Array((()=>2, inputAOfForming),(()=>2, inputBOfForming)), Array((()=>2,outputContainerOfForming)), identity, "forming", verbose)

  val formedContainerStoragePlace = lIFOStorage(1000000000, Nil, "containerStoragePlace", verbose, false)
  val trashContainerStoragePlace = lIFOStorage(1000000000, Nil, "TrashContainerStoragePlace", verbose, false)
  val transportingOutputContainerFromForming = splittingSingleBatchProcess(m, 30,
    Array((()=>120, outputContainerOfForming)),
    Array(Array((()=>1,formedContainerStoragePlace)),
      Array((()=>1,trashContainerStoragePlace))), choseZeroOne, "transportingOutputContainerFromForming", verbose)

  val myStore = metricsStore(List(
    (mult(completedBatchCount(dieCuttingPartA),totalPut(outputSlotOfDieCuttingPArtA)),"a stupid metric, to test the stuff"),
    (cumulatedDuration(empty(rawMaterialStorage)),"duration of empty raw material storage"),
    (empty(rawMaterialStorage),"is raw material storage empty? (at the end of the trace)"),
    (cumulatedDuration(not(running(forming))), "summed duration of forming being inactive"),
    (cumulatedDuration(not(hasBeen(running(forming)))), "summed duration of forming being inactive at the start of the trace"),
    (culumatedDurationNotStart(not(running(forming))), "summed duration of forming being inactive, not counting initial"),
    (maxOnHistory(stockLevel(rawMaterialStorage)),"max content of raw material storage"),
    (minOnHistory(stockLevel(rawMaterialStorage)),"min content of raw material storage"),
    (avgOnHistory(relativeStockLevel(rawMaterialStorage)), "avg relative stock level of raw material storage")
  ), verbose)

  m.simulate(8*60*60, verbose,()=>myStore.updateMetricsIfNeeded(m.clock()))
  myStore.finish(m.clock())

  println(m)
  println(rawMaterialStorage)
  println("part A of the process")
  println(transportingToDieCutterA)
  println(inputFeederOfDieCuttingPartA)
  println(outputSlotOfDieCuttingPArtA)
  println(dieCuttingPartA)

  println(outputBeltFromDieCuttingA)
  println(bufferForDieA)
  println(transportingFromBufferA)
  println("part B of the process")
  println(transportingToDieCutterB)
  println(inputFeederOfDieCuttingPartB)
  println(outputSlotOfDieCuttingPArtB)
  println(dieCuttingPartB)
  println(outputBeltFromDieCuttingB)
  println(bufferForDieB)
  println(transportingFromBufferB)
  println("forming")
  println(inputAOfForming)
  println(inputBOfForming)
  println(forming)
  println(outputContainerOfForming)
  println(transportingOutputContainerFromForming)
  println(formedContainerStoragePlace )
  println(trashContainerStoragePlace )

  println(myStore)
  println("duration:" + (System.currentTimeMillis - starttime))
}
