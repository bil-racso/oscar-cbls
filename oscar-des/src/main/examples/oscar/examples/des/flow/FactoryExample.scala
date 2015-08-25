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
  val transportingToDieCutterA = SingleBatchProcess(m, 15*60, Array((()=>1, rawMaterialStorage)), Array((()=>100,inputFeederOfDieCuttingPartA)), "transportingToDieCutterA", noTransform, verbose)

  val outputSlotOfDieCuttingPArtA = new FIFOStorage[Items](400,Nil,"outputSlotOfDieCuttingPArtA", verbose,false)
  val dieCuttingPartA = SingleBatchProcess(m, 10, Array((()=>1, inputFeederOfDieCuttingPartA)), Array((()=>4,outputSlotOfDieCuttingPArtA)), "dieCuttingPartA", noTransform, verbose)


  val metricsStore = new MetricsStore(List(
    (Mult(BatchCount(dieCuttingPartA),TotalPut(outputSlotOfDieCuttingPArtA)),"aMetric"),
    (CumulatedDuration(Empty(outputSlotOfDieCuttingPArtA),ModelTime(m)),"anotherMetric")), verbose)

  /*
  val bufferForDieA =  new FIFOStorage[Items](500,Nil,"bufferForDieA", verbose,false)
  val outputBeltFromDieCuttingA = new ConveyerBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtA)), List((1, bufferForDieA)), "outputBeltFromDieCuttingA", verbose)

  val inputAOfForming = new Storage(2, 0, "inputAOfForming", verbose)
  val transportingFromBufferA = new ConveyerBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieA)), List((1, inputAOfForming)), "transportingFromBufferA", verbose)

  //we consider here individual "already cut" dies although they are still aggregated into a single coil
  val inputFeederOfDieCuttingPartB = new Storage(100, 0, "inputFeederOfDieCuttingPartB", verbose)

  //takes 15 minutes to transport a coil, and install it
  val transportingToDieCutterB = SingleBatchProcess(m, 15*60, List((1, rawMaterialStorage)), List((100,inputFeederOfDieCuttingPartB)), "transportingToDieCutterB", verbose)

  val outputSlotOfDieCuttingPArtB = new Storage(4, 0, "outputSlotOfDieCuttingPArtB", verbose)
  val dieCuttingPartB = SingleBatchProcess(m, 10, List((1, inputFeederOfDieCuttingPartB)), List((4,outputSlotOfDieCuttingPArtB)), "dieCuttingPartB", verbose)

  val bufferForDieB = new Storage(500, 0, "outputSlotOfDieCuttingPArtA", verbose)
  val outputBeltFromDieCuttingB = new ConveyerBeltProcess(m, 20, 0.5.toFloat , List((1, outputSlotOfDieCuttingPArtB)), List((1, bufferForDieB)), "outputBeltFromDieCuttingB", verbose)

  val inputBOfForming = new Storage(2, 0, "inputBOfForming", verbose)
  val transportingFromBufferB = new ConveyerBeltProcess(m, 20, 0.5.toFloat , List((1, bufferForDieB)), List((1, inputBOfForming)), "transportingFromBufferB", verbose)

  val outputContainerOfForming = new Storage(120, 0, "outputContainerOfForming", verbose)
  val forming = SingleBatchProcess(m, 30, List((2, inputAOfForming),(2, inputBOfForming)), List((2,outputContainerOfForming)), "forming", verbose)

  val formedContainerStoragePlace = new Storage(1000000000, 0, "containerStoragePlace", verbose)
  val trashContainerStoragePlace = new Storage(1000000000, 0, "TrashContainerStoragePlace", verbose)
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

  println(metricsStore)
  /*
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
*/
  println("duration:" + (System.currentTimeMillis - starttime))

}
