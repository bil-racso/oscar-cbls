package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core.{Properties, Inputter}
import oscar.des.flow.core.ItemClassHelper._

import scala.collection.immutable.SortedMap

//TODO: remove Activable, ActivableProcess is enough
abstract class Activable{
  def setUnderControl()
  def activate(intensity:Int)
}

abstract class ActivableProcess(val name:String, verbosity:String=>Unit, val id:Int) extends Activable{
  def isRunning:Boolean
  def completedBatchCount(outputPort:Int = -1):Int
  def startedBatchCount:Int
  def totalWaitDuration:Double

  var properties:Properties = new Properties

  var productionBatch:LIFOStorage = null

  override def setUnderControl(){
    productionBatch = new LIFOStorage(Int.MaxValue,List.empty,"productionWindow_" + this.name, verbosity, false,-1)
    addPreliminaryInput(productionBatch)
  }

  override def activate(intensity: Int): Unit ={
    productionBatch.put(intensity,zeroItemClass)({()=>})
  }

  def addPreliminaryInput(preliminary:Storage)

  def cloneReset(newModel:Model,storages:SortedMap[Storage,Storage]):ActivableProcess
}

abstract class ActivableAtomicProcess(name:String, verbosity:String=>Unit, id:Int) extends ActivableProcess(name,verbosity,id){

  def myInput:Inputter

  override def addPreliminaryInput(preliminary: Storage) {
    myInput.addPreliminaryInput(preliminary)
  }
}

abstract class ActivableMultipleProcess(name:String, verbosity:String=>Unit, id:Int) extends ActivableProcess(name,verbosity,id){
  def childProcesses:Iterable[ActivableAtomicProcess]

  override def addPreliminaryInput(preliminary: Storage) {
    for(s <- childProcesses) s.myInput.addPreliminaryInput(preliminary)
  }

  override def isRunning: Boolean = childProcesses.exists(_.isRunning)
  override def completedBatchCount(outputPort:Int = -1): Int = sumIntOnChildren(_.completedBatchCount(outputPort))
  override def startedBatchCount: Int = sumIntOnChildren(_.startedBatchCount)
  override def totalWaitDuration:Double = sumDoubleOnChildren(_.totalWaitDuration)

  private def sumIntOnChildren(f:(ActivableAtomicProcess => Int)) = childProcesses.foldLeft(0)({case (i:Int,a:ActivableAtomicProcess) => i+f(a)})
  private def sumDoubleOnChildren(f:(ActivableAtomicProcess => Double)) = childProcesses.foldLeft(0.0)({case (i:Double,a:ActivableAtomicProcess) => i+f(a)})

}

