package oscar.des.flow.lib

import oscar.des.flow.core.Inputter
import oscar.des.flow.core.ItemClassHelper._

abstract class Activable{
  def setUnderControl()
  def activate(intensity:Int)
}

abstract class ActivableProcess(name:String, verbose:Boolean) extends Activable{
  def isRunning:Boolean
  def batchCount:Int
  def totalWaitDuration():Double

  var productionBatch:LIFOStorage[Items] = null;

  override def setUnderControl(){
    productionBatch = new LIFOStorage[Items](Int.MaxValue,List.empty,"productionWindow_" + this.name, verbose, false)
    addPreliminaryInput(productionBatch)
  }

  override def activate(intensity: Int): Unit ={
    productionBatch.put(intensity,zeroItemClass)({()=>})
  }

  def addPreliminaryInput(preliminary:Storage[Items])
}

abstract class ActivableAtomicProcess(name:String, verbose:Boolean) extends ActivableProcess(name,verbose){

  def myInput:Inputter

  override def addPreliminaryInput(preliminary: Storage[Items]) {
    myInput.addPreliminaryInput(preliminary)
  }
}

abstract class ActivableMultipleProcess(name:String, verbose:Boolean) extends ActivableProcess(name,verbose){
  def childProcesses:Iterable[ActivableAtomicProcess]

  override def addPreliminaryInput(preliminary: Storage[Items]) {
    for(s <- childProcesses) s.myInput.addPreliminaryInput(preliminary)
  }

  override def isRunning: Boolean = childProcesses.exists(_.isRunning)
  override def batchCount: Int = sumIntOnChildren(_.batchCount)
  override def totalWaitDuration():Double = sumDoubleOnChildren(_.totalWaitDuration)

  private def sumIntOnChildren(f:(ActivableAtomicProcess => Int)) = childProcesses.foldLeft(0)({case (i:Int,a:ActivableAtomicProcess) => i+f(a)})
  private def sumDoubleOnChildren(f:(ActivableAtomicProcess => Double)) = childProcesses.foldLeft(0.0)({case (i:Double,a:ActivableAtomicProcess) => i+f(a)})
}

