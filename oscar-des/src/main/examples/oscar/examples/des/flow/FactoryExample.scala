package oscar.examples.des

import oscar.des.engine.Model
import oscar.des.flow.core.{AttributeSet, AttributeDefinitions}
import oscar.des.flow.lib.MetricsStore
import oscar.des.flow.modeling.FactorySimulationHelper
import scala.collection.immutable.SortedSet
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
  val verbosity = (s:String) => println(s)

  val standardItemClass = zeroItemClass
  val allAttributes = new AttributeDefinitions("sampleAttribute", "anotherOne", "CheapSteel","expensiveSteel")
  val initialRawBatch = allAttributes.getN(0)
  val choseZeroOne = iTE(attributeTerminal(initialRawBatch),outputValue(discreteChoice(List((0,0.5),(1,0.5)))),outputValue(constantPort(1)))

  //time unit is the second
  val rawMaterialStorage = fIFOStorage(200,List((40,AttributeSet(SortedSet(initialRawBatch),allAttributes).itemClass)),"rawMaterialStorage", verbosity,false,attributes=allAttributes)

  val rawSupplier = singleBatchProcess(m, 5000, Array(), Array((()=>1,rawMaterialStorage)), identity, "rawSupplier", verbosity,attributes=allAttributes)
  val ordering = onLowerThreshold(rawMaterialStorage,
    m,
    rawSupplier,
    20,
    (size:Int) => 20,
    verbosity,
    0,
    "orderingPolicy")

  val inputFeederOfDieCuttingPartA = fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA", verbosity,false,attributes=allAttributes)
  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = singleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), identity, "transportingToDieCutterA", verbosity,attributes=allAttributes)

  val outputSlotOfDieCuttingPArtA = lIFOStorage(400,Nil,"outputSlotOfDieCuttingPArtA", verbosity,false,attributes=allAttributes)
  val dieCuttingPartA = singleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), identity, "dieCuttingPartA", verbosity,attributes=allAttributes)

  val outputOfQAAfterCutting = fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA", verbosity,false,attributes=allAttributes)
  val trashContainerForRejectedCutItems = lIFOStorage(1000000000, Nil, "trashContainerForRejectedCutItems", verbosity, false,attributes=allAttributes)
  val qAOnInputFeeder = splittingSingleBatchProcess(m, 30,
    Array((()=>1, outputSlotOfDieCuttingPArtA)),
    Array(Array((()=>1,outputOfQAAfterCutting)),
      Array((()=>1,trashContainerForRejectedCutItems))), choseZeroOne, "QAAterSp", verbosity,attributes=allAttributes)

  val bufferForDieA =  fIFOStorage(500,Nil,"bufferForDieA", verbosity,false,attributes=allAttributes)
  val outputBeltFromDieCuttingA = conveyorBeltProcess(m, 20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtA)), Array((()=>1, bufferForDieA)), identity, "outputBeltFromDieCuttingA", verbosity,attributes=allAttributes)

  val inputAOfForming = fIFOStorage(2, Nil, "inputAOfForming", verbosity, false,attributes=allAttributes)
  val transportingFromBufferA = conveyorBeltProcess(m, 20, 0.5.toFloat , Array((()=>1, bufferForDieA)), Array((()=>1, inputAOfForming)), identity, "transportingFromBufferA", verbosity,attributes=allAttributes)

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = fIFOStorage(100, Nil, "inputFeederOfDieCuttingPartB", verbosity, false,attributes=allAttributes)

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = singleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartB)), identity, "transportingToDieCutterB", verbosity,attributes=allAttributes)

  val outputSlotOfDieCuttingPArtB = lIFOStorage(4, Nil, "outputSlotOfDieCuttingPArtB", verbosity, true,attributes=allAttributes)
  val dieCuttingPartB = singleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartB)), Array((()=>4,outputSlotOfDieCuttingPArtB)), identity, "dieCuttingPartB", verbosity,attributes=allAttributes)

  val bufferForDieB = lIFOStorage(500, Nil, "outputSlotOfDieCuttingPArtA", verbosity, false,attributes=allAttributes)
  val outputBeltFromDieCuttingB = conveyorBeltProcess(m, 20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtB)), Array((()=>1, bufferForDieB)), identity, "outputBeltFromDieCuttingB", verbosity,attributes=allAttributes)

  val inputBOfForming = lIFOStorage(2, Nil, "inputBOfForming", verbosity, false,attributes=allAttributes)
  val transportingFromBufferB = conveyorBeltProcess(m, 20, 0.5.toFloat , Array((()=>1, bufferForDieB)), Array((()=>1, inputBOfForming)), identity, "transportingFromBufferB", verbosity,attributes=allAttributes)

  val outputContainerOfForming = lIFOStorage(120, Nil, "outputContainerOfForming", verbosity,false,attributes=allAttributes)
  val forming = singleBatchProcess(m, 30, Array((()=>2, inputAOfForming),(()=>2, inputBOfForming)), Array((()=>2,outputContainerOfForming)), identity, "forming", verbosity,attributes=allAttributes)

  val formedContainerStoragePlace = lIFOStorage(1000000000, Nil, "containerStoragePlace", verbosity, false,attributes=allAttributes)
  val trashContainerStoragePlace = lIFOStorage(1000000000, Nil, "TrashContainerStoragePlace", verbosity, false,attributes=allAttributes)
  val transportingOutputContainerFromForming = splittingSingleBatchProcess(m, 30,
    Array((()=>120, outputContainerOfForming)),
    Array(Array((()=>1,formedContainerStoragePlace)),
      Array((()=>1,trashContainerStoragePlace))), choseZeroOne, "transportingOutputContainerFromForming", verbosity,attributes=allAttributes)

  val myStore = new MetricsStore(List(
    ("a stupid metric, to test the stuff",mult(completedBatchCount(dieCuttingPartA),totalPut(outputSlotOfDieCuttingPArtA,None))),
    ("duration of empty raw material storage", cumulatedDuration(empty(rawMaterialStorage))),
    ("summed duration of forming being inactive at the start of the trace", cumulatedDuration(not(hasBeen(running(forming))))),
    ("is raw material storage empty? (at the end of the trace)",empty(rawMaterialStorage)),
    ("summed duration of forming being inactive",cumulatedDuration(not(running(forming)))),
    ("summed duration of forming being inactive, not counting initial",culumatedDurationNotStart(not(running(forming)))),
    ("max content of raw material storage",maxOnHistory(stockLevel(rawMaterialStorage))),
    ("max content of cheap steel in raw material storage",maxOnHistory(stockLevel(rawMaterialStorage,Some(attributeTerminal(allAttributes.get("CheapSteel")))))),
    ("max content of expensive steel in raw material storage",maxOnHistory(stockLevel(rawMaterialStorage,Some(attributeTerminal(allAttributes.get("expensiveSteel")))))),
    ("min content of raw material storage",minOnHistory(stockLevel(rawMaterialStorage,None))),
    ("avg relative stock level of raw material storage",avgOnHistory(relativeStockLevel(rawMaterialStorage))),
    ("avg  stock level of raw material storage",avgOnHistory(stockLevel(rawMaterialStorage,None))),
    ("toto",ponderateWithDuration(stockLevel(rawMaterialStorage,None)))
  ), verbosity)

  m.simulate(8*60*60, verbosity,()=>{myStore.updateMetricsIfNeeded(m.clock());false})
  myStore.finish(m.clock())





  println(m)
  println(rawMaterialStorage)
  println("part A of the process")
  println(transportingToDieCutterA)
  println(inputFeederOfDieCuttingPartA)
  println(outputSlotOfDieCuttingPArtA)
  println(dieCuttingPartA)

  println(qAOnInputFeeder)
  println(trashContainerForRejectedCutItems)

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
  println("duration:" + (System.currentTimeMillis - starttime) + "ms")
}

