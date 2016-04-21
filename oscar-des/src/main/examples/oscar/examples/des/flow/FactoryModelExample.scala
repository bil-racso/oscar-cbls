package oscar.examples.des.flow

import oscar.des.engine.Model
import oscar.des.flow.core.{AttributeDefinitions, AttributeSet}
import oscar.des.flow.lib._
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

  val identityFunctionString = ""
  
  val verbosity = null //(s:String) => println(s)

  val fm = new FactoryModel(verbosity)

  val standardItemClass = zeroItemClass
  val allAttributes = new AttributeDefinitions("sampleAttribute", "anotherOne", "CheapSteel","expensiveSteel")
  val initialRawBatch = allAttributes.getN(0)
  val choseZeroOne = iTE(attributeTerminal(initialRawBatch),outputValue(discreteChoice(List((0,0.5),(1,0.5)))),outputValue(constantPort(1)))
  val choseZeroOneString = "if CheapSteel then outputPort 0 weight 0.5 outputPort 1 weight 0.5 else outputPort 0 weight 0.9 outputPort 1 weight 0.1"

  //time unit is the second
  val rawMaterialStorage = fm.fIFOStorage(200,List((40,AttributeSet(SortedSet(initialRawBatch),allAttributes).itemClass)),"rawMaterialStorage",false,"0")

  val rawSupplier = fm.singleBatchProcess(5000, Array(), Array((()=>1,rawMaterialStorage)), identityFunctionString, "rawSupplier")
  val ordering = fm.onLowerThreshold(rawMaterialStorage,
    rawSupplier,
    20,
    (size:Int) => 20,
    0,
    "orderingPolicy")

  val inputFeederOfDieCuttingPartA = fm.fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA",false,"0")
  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = fm.singleBatchProcess(15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), identityFunctionString, "transportingToDieCutterA")

  val outputSlotOfDieCuttingPArtA = fm.lIFOStorage(400,Nil,"outputSlotOfDieCuttingPArtA",false,"0")
  val dieCuttingPartA = fm.singleBatchProcess(10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), identityFunctionString, "dieCuttingPartA")

  val outputOfQAAfterCutting = fm.fIFOStorage(100,Nil,"inputFeederOfDieCuttingPartA",false,"0")
  val trashContainerForRejectedCutItems = fm.lIFOStorage(1000000000, Nil, "trashContainerForRejectedCutItems", false,"0")
  val qAOnInputFeeder = fm.splittingSingleBatchProcess(30,
    Array((()=>1, outputSlotOfDieCuttingPArtA)),
    Array(Array((()=>1,outputOfQAAfterCutting)),
      Array((()=>1,trashContainerForRejectedCutItems))), choseZeroOneString, "QAAterSp")

  val bufferForDieA =  fm.fIFOStorage(500,Nil,"bufferForDieA",false,"0")
  val outputBeltFromDieCuttingA = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtA)), Array((()=>1, bufferForDieA)), identityFunctionString, "outputBeltFromDieCuttingA")

  val inputAOfForming = fm.fIFOStorage(2, Nil, "inputAOfForming", false,"0")
  val transportingFromBufferA = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, bufferForDieA)), Array((()=>1, inputAOfForming)), identityFunctionString, "transportingFromBufferA")

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = fm.fIFOStorage(100, Nil, "inputFeederOfDieCuttingPartB", false,"0")

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = fm.singleBatchProcess(15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartB)), identityFunctionString, "transportingToDieCutterB")

  val outputSlotOfDieCuttingPArtB = fm.lIFOStorage(4, Nil, "outputSlotOfDieCuttingPArtB", true,"0")
  val dieCuttingPartB = fm.singleBatchProcess(10, Array((()=>1, inputFeederOfDieCuttingPartB)), Array((()=>4,outputSlotOfDieCuttingPArtB)), identityFunctionString, "dieCuttingPartB")

  val bufferForDieB = fm.lIFOStorage(500, Nil, "outputSlotOfDieCuttingPArtA", false,"0")
  val outputBeltFromDieCuttingB = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, outputSlotOfDieCuttingPArtB)), Array((()=>1, bufferForDieB)), identityFunctionString, "outputBeltFromDieCuttingB")

  val inputBOfForming = fm.lIFOStorage(2, Nil, "inputBOfForming", false,"0")
  val transportingFromBufferB = fm.conveyorBeltProcess(20, 0.5.toFloat , Array((()=>1, bufferForDieB)), Array((()=>1, inputBOfForming)), identityFunctionString, "transportingFromBufferB")

  val outputContainerOfForming = fm.lIFOStorage(120, Nil, "outputContainerOfForming",false,"0")
  val forming = fm.singleBatchProcess(30, Array((()=>2, inputAOfForming),(()=>2, inputBOfForming)), Array((()=>2,outputContainerOfForming)), identityFunctionString, "forming")

  val formedContainerStoragePlace = fm.lIFOStorage(1000000000, Nil, "containerStoragePlace", false,"0")
  val trashContainerStoragePlace = fm.lIFOStorage(1000000000, Nil, "TrashContainerStoragePlace", false,"0")
  val transportingOutputContainerFromForming = fm.splittingSingleBatchProcess(30,
    Array((()=>120, outputContainerOfForming)),
    Array(Array((()=>1,formedContainerStoragePlace)),
      Array((()=>1,trashContainerStoragePlace))), choseZeroOneString, "transportingOutputContainerFromForming")


  fm.setQueries(List(
    ("a stupid metric, to test the stuff",mult(completedBatchCount(dieCuttingPartA),totalPut(outputSlotOfDieCuttingPArtA,None))),
    ("duration of empty raw material storage", cumulatedDuration(empty(rawMaterialStorage))),
    ("summed duration of forming being inactive at the start of the trace", cumulatedDuration(not(hasBeen(running(forming))))),
    ("is raw material storage empty? (at the end of the trace)",empty(rawMaterialStorage)),
    ("summed duration of forming being inactive",cumulatedDuration(not(running(forming)))),
    ("summed duration of forming being inactive, not counting initial",culumatedDurationNotStart(not(running(forming)))),
    ("max content of raw material storage",maxOnHistory(stockLevel(rawMaterialStorage,None))),
    ("min content of raw material storage",minOnHistory(stockLevel(rawMaterialStorage,None))),
    ("avg relative stock level of raw material storage",avgOnHistory(relativeStockLevel(rawMaterialStorage))),
    ("avg stock level of raw material storage",avgOnHistory(stockLevel(rawMaterialStorage,None))),
    ("toto",ponderateWithDuration(stockLevel(rawMaterialStorage,None))),
    ("stock history",doubleHistoryExpr(stockLevel(rawMaterialStorage,None)))
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

