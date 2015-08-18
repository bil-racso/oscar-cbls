package oscar.des.flow.lib

import oscar.des.flow.core.Inputter


abstract class Activable{
  def setUnderControl()
  def activate(intensity:Int)
}


abstract class ActivableAtomicProcess(name:String, verbose:Boolean) extends Activable{
  var productionBatch:LIFOStorage[Items] = null;
  def myInput:Inputter

  override def setUnderControl(){
    productionBatch = new LIFOStorage[Items](Int.MaxValue,List.empty,"productionWindow_" + this.name, verbose, false)
    myInput.addPreliminaryInput(productionBatch)
  }

  override def activate(intensity: Int): Unit ={
    productionBatch.put(intensity,null)
  }
}

abstract class ActivableMultipleProcess(name:String, verbose:Boolean) extends Activable{
  var productionBatch:LIFOStorage[Items] = null;
  def childProcesses:Iterable[ActivableAtomicProcess]

  override def setUnderControl(){
    productionBatch = new LIFOStorage[Items](Int.MaxValue,List.empty,"productionWindow_" + this.name, verbose, false)
    for(s <- childProcesses) s.myInput.addPreliminaryInput(productionBatch)
  }

  override def activate(intensity: Int): Unit ={
    productionBatch.put(intensity,null)
  }
}