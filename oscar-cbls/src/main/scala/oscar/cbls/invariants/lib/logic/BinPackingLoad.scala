package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{InvariantHelper, _}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.numeric.Sum

/**
  * Created by gustavbjordal on 27/05/16.
  */
case class BinPackingLoad(items: Array[IntValue], itemsizes: Array[Int]) extends Invariant {
  for (v <- items.indices) registerStaticAndDynamicDependency(items(v),v)

  finishInitialization()

  val (minVal,maxVal) = InvariantHelper.getMinMaxBounds(items)
  assert(minVal==0)

  private val bincontents = Cluster.MakeDense(items).clusters
  private val binfilling = bincontents.map(bincontent => Sum(itemsizes,bincontent))


  def PostInvariants(loads:Array[CBLSIntVar]) = {
    for(i <- loads.indices)
      loads(i) <== binfilling(i)

  }


  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }
}