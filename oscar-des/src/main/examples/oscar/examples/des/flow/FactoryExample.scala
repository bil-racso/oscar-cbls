package oscar.examples.des

import oscar.des.engine.Model
import oscar.des.flow.core.Identity
import oscar.des.flow.lib._
import oscar.des.flow.core.ItemClassHelper._

/**
 * Created by rdl on 2/02/2015.
 */

object FactoryExample extends App with HelperForProcess{

  val starttime = System.currentTimeMillis

  def probability(p:Double)():Boolean = {
    math.random < p
  }
  def choose(r:Range)():Int = {
    r.apply ((math.random * r.size).toInt)
  }

  val m = new Model
  val verbose = false

  val standardItemClass:ItemClass = zeroItemClass
  val noTransform = mkFunction(Identity())

  //time unit is the second
  val rawMaterialStorage = new  FIFOStorage[Items](200,List((40,standardItemClass)),"rawMaterialStorage", verbose,false)


  val rawSupplier = new SingleBatchProcess(m, 5000, Array(), Array((()=>1,rawMaterialStorage)), noTransform, "rawSupplier", verbose)
  val ordering = new OnStockThreshold[Items](rawMaterialStorage,
    m,
    rawSupplier,
    20,
    (size:Int) => 20,
    true,
    0,
    "orderingPolicy")

  val inputFeederOfDieCuttingPartA = new FIFOStorage[Items](100,Nil,"inputFeederOfDieCuttingPartA", verbose,false)

  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = SingleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), noTransform, "transportingToDieCutterA", verbose)

  val outputSlotOfDieCuttingPArtA = new LIFOStorage[Items](400,Nil,"outputSlotOfDieCuttingPArtA", verbose,false)
  val dieCuttingPartA = SingleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), noTransform, "dieCuttingPartA", verbose)

  val bufferForDieA =  new FIFOStorage[Items](500,Nil,"bufferForDieA", verbose,false)
  val outputBeltFromDieCuttingA = new ConveyorBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtA)), List((1, bufferForDieA)), noTransform, "outputBeltFromDieCuttingA", verbose)


  val inputAOfForming = new FIFOStorage[Items](2, Nil, "inputAOfForming", verbose, false)
  val transportingFromBufferA = new ConveyorBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieA)), List((1, inputAOfForming)), noTransform, "transportingFromBufferA", verbose)

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = new FIFOStorage[Items](100, Nil, "inputFeederOfDieCuttingPartB", verbose, false)

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = SingleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartB)), noTransform, "transportingToDieCutterB", verbose)

  val outputSlotOfDieCuttingPArtB = new LIFOStorage[Items](4, Nil, "outputSlotOfDieCuttingPArtB", verbose, true)
  val dieCuttingPartB = SingleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartB)), Array((()=>4,outputSlotOfDieCuttingPArtB)), noTransform, "dieCuttingPartB", verbose)

  val bufferForDieB = new LIFOStorage[Items](500, Nil, "outputSlotOfDieCuttingPArtA", verbose, false)
  val outputBeltFromDieCuttingB = new ConveyorBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtB)), List((1, bufferForDieB)), noTransform, "outputBeltFromDieCuttingB", verbose)

  val inputBOfForming = new LIFOStorage[Items](2, Nil, "inputBOfForming", verbose, false)
  val transportingFromBufferB = new ConveyorBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieB)), List((1, inputBOfForming)), noTransform, "transportingFromBufferB", verbose)

  val outputContainerOfForming = new LIFOStorage[Items](120, Nil, "outputContainerOfForming", verbose,false)
  val forming = SingleBatchProcess(m, 30, Array((()=>2, inputAOfForming),(()=>2, inputBOfForming)), Array((()=>2,outputContainerOfForming)), noTransform, "forming", verbose)

  val formedContainerStoragePlace = new LIFOStorage[Items](1000000000, Nil, "containerStoragePlace", verbose, false)
  val trashContainerStoragePlace = new LIFOStorage[Items](1000000000, Nil, "TrashContainerStoragePlace", verbose, false)
  val transportingOutputContainerFromForming = SplittingSingleBatchProcess(m, 30,
    Array((()=>120, outputContainerOfForming)),
    Array(Array((()=>1,formedContainerStoragePlace)),
      Array((()=>1,trashContainerStoragePlace))), (i:ItemClass) => (choose(0 to 1),i), "transportingOutputContainerFromForming", verbose)

  val metricsStore = new MetricsStore(List(
    (Mult(CompletedBatchCount(dieCuttingPartA),TotalPut(outputSlotOfDieCuttingPArtA)),"a stupid metric, to test the stuff"),
    (CumulatedDuration(Empty(rawMaterialStorage)),"duration of empty raw material storage"),
    (Empty(rawMaterialStorage),"is raw material storage empty? (at the end of the trace)"),
    (CumulatedDuration(Not(Running(forming))), "cumulated duration of forming being inactive"),
    (CumulatedDuration(Not(HasBeen(Running(forming)))), "cumulated duration of forming being inactive at the start of the trace"),
    (CulumatedDurationNotStart(Not(Running(forming))), "cumulated duration of forming being inactive, not counting initial"),
    (MaxOnHistory(StockLevel(rawMaterialStorage)),"max content of raw material storage"),
      (MinOnHistory(StockLevel(rawMaterialStorage)),"min content of raw material storage")

  ), verbose)
  
  m.simulate(8*60*60, verbose,()=>metricsStore.updateMetricsIfNeeded(m.clock()))
  metricsStore.finish(m.clock())

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

  println(metricsStore)
  println("duration:" + (System.currentTimeMillis - starttime))
}
