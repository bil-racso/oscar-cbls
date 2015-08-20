package oscar.des.flow.lib

import oscar.des.flow.core.Inputter


abstract class Activable{
  def setUnderControl()
  def activate(intensity:Int)
}

abstract class ActivableProcess(name:String, verbose:Boolean) extends Activable{
  def isRunning:Boolean
  var productionBatch:LIFOStorage[Items] = null;

  override def setUnderControl(){
    productionBatch = new LIFOStorage[Items](Int.MaxValue,List.empty,"productionWindow_" + this.name, verbose, false)
    addPreliminaryInput(productionBatch)
  }

  override def activate(intensity: Int): Unit ={
    productionBatch.put(intensity,null)
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
}

