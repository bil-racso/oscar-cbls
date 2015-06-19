package oscar.cp.core.delta

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint

class DeltaIntVar(x: CPIntVar, filter: SnapshotIntVar => CPOutcome, idempot: Boolean = false, priority: Int) extends Constraint(x.store, "DeltaVarInt") {
  
  idempotent = idempot
  priorityL2 = priority
  
  private[this] val snapshot = x.snapshot
  
  @inline final def getSnapshot = snapshot
  
  s.onPop { snapshot.update() }
  
  override def snapshotVarInt() {
    super.snapshotVarInt()
    snapshot.update()
  }
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    x.callPropagateWhenDomainChanges(this)
    CPOutcome.Suspend
  }
  
  override def propagate() = filter(snapshot)
  
  def changed() = x.changed(snapshot)
  def size() = x.deltaSize(snapshot)
  def values() = x.delta(snapshot.oldMin,snapshot.oldMax,snapshot.oldSize)
  def fillArray(arr: Array[Int]): Int = x.fillDeltaArray(snapshot.oldMin, snapshot.oldMax, snapshot.oldSize, arr)
  def minChanged() = x.minChanged(snapshot)
  def maxChanged() = x.maxChanged(snapshot)
  def oldMin() = x.oldMin(snapshot)
  def oldMax() = x.oldMax(snapshot)
}