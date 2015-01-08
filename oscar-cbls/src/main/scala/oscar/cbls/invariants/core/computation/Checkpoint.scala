/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.propagation.KeyForElementRemoval

trait Checkpointing extends Store{

  private val recorder = new ChangeRecorder(this)

  /** defines a new checkpoint
    * notice that all preceding checkpoints can still be restored ass well,
    * @return the checkpoint
    */
  def defineCheckpoint():Checkpoint = {

    recorder.recordingCheckpoint = new Checkpoint(recorder.recordingCheckpoint)

    if(recorder.active){
      recorder.reactivateForNewCheckpoint
    }else{
      recorder.active = true;
    }
    recorder.recordingCheckpoint
  }

  /** restores the given checkpoint.
    * notice that all preceding checkpoints can still be restored ass well,
    * not the checkpoints that have been defined later
    * notice that when a checkpoint is restored, the recording mechanism is still active and starts at this checkpoint
    * @param c
    */
  def rollBackTo(c:Checkpoint){
    require(recorder.active)
    while(recorder.recordingCheckpoint != c && recorder.recordingCheckpoint != null){
      recorder.recordingCheckpoint.rollBackSingleCheckpoint()
      recorder.recordingCheckpoint = recorder.recordingCheckpoint.prevCheckpoint
    }

    if(recorder.recordingCheckpoint != c){
      throw new Error("checkpoint is not live, could not be restored (state was restored to the oldest checkpoint)")
    }else{
      recorder.recordingCheckpoint.rollBackSingleCheckpoint()
      recorder.recordingCheckpoint.clear()
    }
    recorder.reactivateForNewCheckpoint
  }

  /** when you are sure of where you are, you can forget about past chekpoints
    *
    * @param c
    */
  def dropAllCheckpointsBefore(c:Checkpoint){
    require(recorder.active)
    c.prevCheckpoint = null
  }

  /** defines a new checkpoint, and drop all live ones.
    * it is the most efficient one
    * @return a new checkpoint
    */
  def defineCheckpointAndDropAllOthers():Checkpoint = {
    require(recorder.active)
    val toReturn = defineCheckpoint()
    dropAllCheckpointsBefore(toReturn)
    toReturn
  }

  /** this drops all live checkpoints
    * and deactivate the checkpointing mechanism
    * (notice that activating and deactivating checkpointing takes time)
    */
  def turnOffCheckpointing(){
    recorder.active = false
    recorder.recordingCheckpoint = null
  }

  /**
   * @return the list of live checkpoints, that can be restored
   */
  def liveCheckpoints:List[Checkpoint] = {
    def accumulate(c:Checkpoint):List[Checkpoint] = {
      if(c == null)List.empty
      else c :: accumulate(c.prevCheckpoint)
    }
    accumulate(recorder.recordingCheckpoint)
  }

  override def close(DropStaticGraph: Boolean){
    performCallsBeforeClose()
    recorder.close()
    super.close(DropStaticGraph)
  }
}

class ChangeRecorder(s:Store) extends Invariant{

  var keys : Array[KeyForElementRemoval] = null

  private var myActive = false
  var recordingCheckpoint:Checkpoint = null
  var changesVariables:List[(Variable,Int)] = List.empty

  def active_=(a:Boolean){
    if(a && !myActive){
      //on active l'enregistrement, qui était désactivé

      keys = Array.tabulate(s.decisionVariables.length)(null)

      var varId = 0;
      for(v <- s.decisionVariables){
        keys(varId) = registerDynamicDependency(v,varId)
        varId +=1
      }
      changesVariables = List.empty
    }else if (!a && myActive){
      //on désactive l'enregistrement
      for(k <- keys) {
        if(k != null) unregisterDynamicDependency(k)
      }
      keys = null
    }

    myActive = a
  }

  def active:Boolean = myActive

  def close() {
    registerStaticDependencies(s.decisionVariables():_*)
  }

  def varHasChanged(variable:Variable, varId:Int){
    unregisterDynamicDependency(keys(varId))
    keys(varId) = null
    changesVariables = (variable,varId) :: changesVariables
  }

  def reactivateForNewCheckpoint(){
    for((variable,varId) <- changesVariables){
      keys(varId) = registerDynamicDependency(variable,varId)
    }
    changesVariables = List.empty
  }

  override def notifyIntChanged(v: CBLSIntVar, i: Int, OldVal: Int, NewVal: Int) {
    val moveForUndo:(Unit=>Unit) = (_ => v := OldVal)
    varHasChanged(v, i)
    recordingCheckpoint.addUndo(moveForUndo)
  }

  override def notifyInsertOn(v: CBLSSetVar, i: Int, value: Int){
    val savedValue = (v.value - i)
    val moveForUndo:(Unit=>Unit) = (_ => v := savedValue)
    varHasChanged(v, i)
    recordingCheckpoint.addUndo(moveForUndo)
  }

  override def notifyDeleteOn(v: CBLSSetVar, i: Int, value: Int){
    val savedValue = (v.value + i)
    val moveForUndo:(Unit=>Unit) = (_ => v := savedValue)
    varHasChanged(v, i)
    recordingCheckpoint.addUndo(moveForUndo)
  }
}

class Checkpoint(var prevCheckpoint:Checkpoint = null){
  private var undoList:List[(Unit=>Unit)] = List.empty

  protected[cbls] def addUndo(op:(Unit=>Unit)){
    undoList = op :: undoList
  }

  protected[cbls] def rollBackSingleCheckpoint(){
    for(op <- undoList) op
    undoList = null //this is to ensure a crash in case we try to undo twice.
  }

  protected[cbls] def clear(){
    undoList = List.empty
  }
}

