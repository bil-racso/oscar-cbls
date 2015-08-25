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

  val m = new Model
  val verbose = false

  val standardItemClass:ItemClass = zeroItemClass
  val noTransform = mkFunction(Identity())

  //time unit is the second
  val rawMaterialStorage = new  FIFOStorage[Items](200,List((200,standardItemClass)),"rawMaterialStorage", verbose,false)

  val inputFeederOfDieCuttingPartA = new FIFOStorage[Items](100,Nil,"inputFeederOfDieCuttingPartA", verbose,false)

  //takes 15 minutes to transport a coil, and install it
  //we suppose that hte coil is decomposed into parts during the transportation, because we have no model of "cutting process"
  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val transportingToDieCutterA = SingleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), noTransform, "transportingToDieCutterA", verbose)

  val outputSlotOfDieCuttingPArtA = new LIFOStorage[Items](400,Nil,"outputSlotOfDieCuttingPArtA", verbose,false)
  val dieCuttingPartA = SingleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), noTransform, "dieCuttingPartA", verbose)

  val metricsStore = new MetricsStore(List(
    (Mult(BatchCount(dieCuttingPartA),TotalPut(outputSlotOfDieCuttingPArtA)),"aMetric"),
    (CumulatedDuration(Empty(outputSlotOfDieCuttingPArtA),ModelTime(m)),"anotherMetric")), verbose)


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

  /*
  val formedContainerStoragePlace = new LIFOStorage[Items](1000000000, Nil, "containerStoragePlace", verbose, false)
  val trashContainerStoragePlace = new LIFOStorage[Items](1000000000, Nil, "TrashContainerStoragePlace", verbose, false)
  val transportingOutputContainerFromForming = FailingSingleBatchProcess(m, 30,
    List((120, outputContainerOfForming)),
    List((1,formedContainerStoragePlace)),
    List((1,trashContainerStoragePlace)), probability(1-0.05), "transportingOutputContainerFromForming", verbose)
*/


  m.simulate(8*60*60, verbose,metricsStore.updateMetricsIfNeeded)
  metricsStore.finish()
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
  /*
  println(transportingOutputContainerFromForming)
  println(formedContainerStoragePlace )
  println(trashContainerStoragePlace )
*/

  println(metricsStore)
  println("duration:" + (System.currentTimeMillis - starttime))

}
