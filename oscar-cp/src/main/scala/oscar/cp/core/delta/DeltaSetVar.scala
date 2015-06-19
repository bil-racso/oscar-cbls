package oscar.cp.core.delta

import oscar.cp.core.CPOutcome
import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.Constraint

abstract class DeltaSetVar(x: CPSetVar,filter: DeltaSetVar => CPOutcome) extends Constraint(x.store, "DeltaVarSet") {
  
  idempotent = true
  
  val sn = new SnapshotVarSet(x)
  s.onPop {
    sn.update()
  }
  
  override def snapshotVarSet() {
    super.snapshotVarSet()
    sn.update()
  }
  
  override def propagate() = filter(this)
  
  def changed() = x.changed(sn)
  def possibleChanged() = x.possibleChanged(sn)
  def requiredChanged() = x.requiredChanged(sn)
  def deltaPossibleSize() = x.deltaPossibleSize(sn)
  def deltaRequiredSize() = x.deltaRequiredSize(sn)
  def deltaPossible(): Iterator[Int] = x.deltaPossible(sn)
  def deltaRequired(): Iterator[Int] = x.deltaRequired(sn)
  
}