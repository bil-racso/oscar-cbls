package oscar.examples.des.flow

import oscar.des.flow.core.{AttributeDefinitions, AttributeSet}
import oscar.des.flow.lib._
import oscar.des.flow.modeling.{AttributeHelper, ListenersHelper}

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

object Attributes extends App with AttributeHelper with ListenersHelper with implicitConvertors{

  def probability(p:Double)():Boolean = {
    math.random < p
  }
  def choose(r:Range)():Int = {
    r.apply ((math.random * r.size).toInt)
  }

  val verbosity = null //(s:String) => println(s)

  val fm = new FactoryModel(verbosity,attributes=new AttributeDefinitions("sampleAttribute", "anotherOne", "CheapSteel","expensiveSteel"))

  val standardItemClass = zeroItemClass
  val choseZeroOne = iTE(attributeTerminal(fm.attributes.get("CheapSteel")),outputValue(discreteChoice(List((0,0.5),(1,0.5)))),outputValue(discreteChoice(List((0,0.9),(1,0.1)))))

  //time unit is the second
  val rawMaterialStorage = fm.fIFOStorage(200,List((40,fm.attributes.getN(0).toItemClass)),"rawMaterialStorage",false,"0")

  val rawSupplier = fm.singleBatchProcess(5000, Array(), Array((()=>1,rawMaterialStorage)), identity, "rawSupplier")
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
  val transportingToDieCutterA = fm.singleBatchProcess(15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), identity, "transportingToDieCutterA")
  //  fm.setQueriesToParse()

  val outputSlotOfDieCuttingPArtA = fm.lIFOStorage(400,Nil,"outputSlotOfDieCuttingPArtA",false,"0")
  val dieCuttingPartA = fm.singleBatchProcess(10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), identity, "dieCuttingPartA")

  val globalOutput = fm.fIFOStorage(100000,Nil,"globalOutput",false,"0")
  val trashContainerForRejectedCutItems = fm.lIFOStorage(1000000000, Nil, "trashContainerForRejectedCutItems", false,"0")
  val qAOnInputFeeder = fm.splittingSingleBatchProcess(30,
    Array((()=>1, outputSlotOfDieCuttingPArtA)),
    Array(Array((()=>1,globalOutput)),
      Array((()=>1,trashContainerForRejectedCutItems))), choseZeroOne, "QAAterSp")

  fm.setQueriesToParse(List(
    ("duration of empty raw material storage", "cumulatedDuration(empty(rawMaterialStorage))"),
    ("is raw material storage empty? (at the end of the trace)","empty(rawMaterialStorage)"),
    ("max content of raw material storage","max(content(rawMaterialStorage))"),
    ("min content of raw material storage","min(content(rawMaterialStorage))"),
    ("max content of cheap steel in the raw material storage","max(content(rawMaterialStorage,CheapSteel))"),
    ("min content of cheap steel in the raw material storage","min(content(rawMaterialStorage,CheapSteel))"),
    ("avg relative stock level of raw material storage","avg(relativeStockLevel(rawMaterialStorage))"),
    ("avg stock level of raw material storage","avg(content(rawMaterialStorage))"),
    ("rejected batches on Qa","completedBatchCount(QAAterSp,1)"),
    ("toto","integral(content(rawMaterialStorage))"),
    ("stock history","record(content(rawMaterialStorage))")
  ))

  println("end build, start run")
  fm.simulate(8*60*60)

  if(verbosity == null) println(fm)

  println("start cloneReset")

  val other = fm.cloneReset

  println("start ssecond run")
  other.simulate(8*60*60)
  if(verbosity == null) println(other)
}

