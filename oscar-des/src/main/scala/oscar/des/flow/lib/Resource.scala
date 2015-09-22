package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.DoublyLinkedList
import oscar.des.flow.core.Fetchable
import oscar.des.flow.core.ItemClassHelper.ItemClass

import scala.collection.immutable.SortedMap

class CumulativeResource(size:Int,management:ResourceAllocationStrategy,usedBy:List[(ActivableProcess,Int)]) extends Fetchable{

  override def fetch(amount: Int)(block: (ItemClass) => Unit): Unit = ???

  /*  allocation strategies:
    first come first serve
      batch round robin
  time round-robin
  notion of skip??
    absolute priorities
*/
}

abstract class ResourceAllocationStrategy(m:Model){
  def enqueueQuery(from:ActivableProcess,amount:Int,grant:()=>Unit)
  def release(by:ActivableProcess)
}

class FirstComeFirstServeStrategy(m:Model,size:Int) extends ResourceAllocationStrategy(m){
  var remainingResources = size
  var usedResources:SortedMap[String,(Int,Activable)] = SortedMap.empty
  val waitingRequests:DoublyLinkedList[(ActivableProcess,Int,() => Unit)] = new DoublyLinkedList[(ActivableProcess,Int,() => Unit)]()

  override def enqueueQuery(from: ActivableProcess, amount: Int, grant: () => Unit){
    waitingRequests.append(from,amount,grant)
    runIfPossible()
  }

  override def release(by: ActivableProcess): Unit ={
    val Some((amount,process)) = usedResources.get(by.name)
    require(process == by)
    remainingResources += amount
    usedResources = usedResources.-(by.name)
    runIfPossible()
  }

  private def runIfPossible(): Unit ={
    if(waitingRequests.nonEmpty){
      val head = waitingRequests.first
      if(head._2 <= remainingResources){
        remainingResources -= head._2
        waitingRequests.deleteFirst
        usedResources += head._1.name -> (head._2,head._3)
        head._3()
        runIfPossible()
      }
    }
  }
}

class BatchRoundRobinStrategy(m:Model,sequence:List[(ActivableProcess,Int)], timeout:Double) extends ResourceAllocationStrategy(m){
  override def enqueueQuery(from: ActivableProcess, amount: ItemClass, grant: () => Unit): Unit = ???

  override def release(by: ActivableProcess): Unit = ???
}

class TimeRoundRobinStrategy(m:Model,sequence:List[(ActivableProcess,Double)],timeout:Double) extends ResourceAllocationStrategy(m){

}

//equivalent priorities are handled as FIFO.
class AbsolutePriorityStrategy(m:Model,priorities: List[(ActivableProcess,Int)]) extends ResourceAllocationStrategy(m){

}

