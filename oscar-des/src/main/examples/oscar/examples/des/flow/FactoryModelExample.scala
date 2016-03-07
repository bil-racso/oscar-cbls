package oscar.examples.des.flow

import oscar.des.engine.Model
import oscar.des.flow.core.{AttributeDefinitions, AttributeSet}
import oscar.des.flow.lib.{implicitConvertors, FactoryModel}
import oscar.des.flow.modeling.{ListenersHelper, AttributeHelper}

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

/**
 * Created by rdl on 2/02/2015.
 */

object FactoryModelExample extends App with AttributeHelper with ListenersHelper with implicitConvertors{

  val starttime = System.currentTimeMillis

  def probability(p:Double)():Boolean = {
    math.random < p
  }
  def choose(r:Range)():Int = {
    r.apply ((math.random * r.size).toInt)
  }

  val verbosity = null //(s:String) => println(s)

  val fm = new FactoryModel(verbosity)

  val standardItemClass = zeroItemClass
  val allAttributes = new AttributeDefinitions("sampleAttribute", "anotherOne", "CheapSteel","expensiveSteel")
  val initialRawBatch = allAttributes.getN(0)
  val choseZeroOne = iTE(attributeTerminal(initialRawBatch),outputValue(discreteChoice(List((0,0.5),(1,0.5)))),outputValue(constantPort(1)))

  //time unit is the second
  val rawMaterialStorage = fm.fIFOStorage(200,List((40,AttributeSet(SortedSet(initialRawBatch),allAttributes).itemClass)),"rawMaterialStorage",false)

  val rawSupplier = fm.singleBatchProcess(5000, Array(), Array((()=>1,rawMaterialStorage)), identity, "rawSupplier")
  val ordering = fm.onLowerThreshold(rawMaterialStorage,
    rawSupplier,
    20,
    (size:Int) => 20,
    0,
    "orderingPolicy")

  val inputFeederOfDieCuttingPartA = fm.fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA",false)
  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = fm.singleBatchProcess(15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), identity, "transportingToDieCutterA")

  val outputSlotOfDieCuttingPArtA = fm.lIFOStorage(400,Nil,"outputSlotOfDieCuttingPArtA",false)
  val dieCuttingPartA = fm.singleBatchProcess(10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), identity, "dieCuttingPartA")

  val outputOfQAAfterCutting = fm.fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA",false)
  val trashContainerForRejectedCutItems = fm.lIFOStorage(1000000000, Nil, "trashContainerForRejectedCutItems", false)
  val qAOnInputFeeder = fm.splittingSingleBatchProcess(30,
    Array((()=>1, outputSlotOfDieCuttingPArtA)),
    Array(Array((()=>1,outputOfQAAfterCutting)),
      Array((()=>1,trashContainerForRejectedCutItems))), choseZeroOne, "QAAterSp")

  val bufferForDieA =  fm.fIFOStorage(500,Nil,"bufferForDieA",false)
  val outputBeltFromDieCuttingA = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtA)), Array((()=>1, bufferForDieA)), identity, "outputBeltFromDieCuttingA")

  val inputAOfForming = fm.fIFOStorage(2, Nil, "inputAOfForming", false)
  val transportingFromBufferA = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, bufferForDieA)), Array((()=>1, inputAOfForming)), identity, "transportingFromBufferA")

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = fm.fIFOStorage(100, Nil, "inputFeederOfDieCuttingPartB", false)

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = fm.singleBatchProcess(15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartB)), identity, "transportingToDieCutterB")

  val outputSlotOfDieCuttingPArtB = fm.lIFOStorage(4, Nil, "outputSlotOfDieCuttingPArtB", true)
  val dieCuttingPartB = fm.singleBatchProcess(10, Array((()=>1, inputFeederOfDieCuttingPartB)), Array((()=>4,outputSlotOfDieCuttingPArtB)), identity, "dieCuttingPartB")

  val bufferForDieB = fm.lIFOStorage(500, Nil, "outputSlotOfDieCuttingPArtA", false)
  val outputBeltFromDieCuttingB = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtB)), Array((()=>1, bufferForDieB)), identity, "outputBeltFromDieCuttingB")

  val inputBOfForming = fm.lIFOStorage(2, Nil, "inputBOfForming", false)
  val transportingFromBufferB = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, bufferForDieB)), Array((()=>1, inputBOfForming)), identity, "transportingFromBufferB")

  val outputContainerOfForming = fm.lIFOStorage(120, Nil, "outputContainerOfForming",false)
  val forming = fm.singleBatchProcess(30, Array((()=>2, inputAOfForming),(()=>2, inputBOfForming)), Array((()=>2,outputContainerOfForming)), identity, "forming")

  val formedContainerStoragePlace = fm.lIFOStorage(1000000000, Nil, "containerStoragePlace", false)
  val trashContainerStoragePlace = fm.lIFOStorage(1000000000, Nil, "TrashContainerStoragePlace", false)
  val transportingOutputContainerFromForming = fm.splittingSingleBatchProcess(30,
    Array((()=>120, outputContainerOfForming)),
    Array(Array((()=>1,formedContainerStoragePlace)),
      Array((()=>1,trashContainerStoragePlace))), choseZeroOne, "transportingOutputContainerFromForming")


  fm.setQueries(List(
    ("a stupid metric, to test the stuff",mult(completedBatchCount(dieCuttingPartA),totalPut(outputSlotOfDieCuttingPArtA))),
    ("duration of empty raw material storage", cumulatedDuration(empty(rawMaterialStorage))),
    ("summed duration of forming being inactive at the start of the trace", cumulatedDuration(not(hasBeen(running(forming))))),
    ("is raw material storage empty? (at the end of the trace)",empty(rawMaterialStorage)),
    ("summed duration of forming being inactive",cumulatedDuration(not(running(forming)))),
    ("summed duration of forming being inactive, not counting initial",culumatedDurationNotStart(not(running(forming)))),
    ("max content of raw material storage",maxOnHistory(stockLevel(rawMaterialStorage))),
    ("min content of raw material storage",minOnHistory(stockLevel(rawMaterialStorage))),
    ("avg relative stock level of raw material storage",avgOnHistory(relativeStockLevel(rawMaterialStorage))),
    ("avg  stock level of raw material storage",avgOnHistory(stockLevel(rawMaterialStorage))),
    ("toto",ponderateWithDuration(stockLevel(rawMaterialStorage)))
  ))

  println("end build, start run")
  fm.simulate(8*60*60)

  println("duration:" + (System.currentTimeMillis - starttime) + "ms")
  if(verbosity == null) println(fm)

  println("start cloneReset")

  val other = fm.cloneReset

  println("start ssecond run")
  other.simulate(8*60*60)
  if(verbosity == null) println(other)
}

