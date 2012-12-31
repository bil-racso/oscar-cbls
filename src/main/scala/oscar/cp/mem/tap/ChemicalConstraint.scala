package oscar.cp.mem.tap

import oscar.cp.core.Constraint
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.reversible.ReversibleInt

/**
 * Constraint Enforcing dominance rules of the Chemical Tanker Problem:
 * Since we try to maximize the total free space, as soon as the total capacity
 * allocated to cargo exceed the volume of this cargo to place we immediately
 * forbid this cargo in other tanks.
 */
class ChemicalConstraint(val cargo: Cargo, val tanks: Array[Tank], val cargos: Array[CPVarInt]) extends Constraint(cargos(0).s) {

  val curCapa = new ReversibleInt(s, 0)

  override def setup(l: CPPropagStrength) = {
    cargos.zipWithIndex.foreach(e => e._1.callValBindIdxWhenBind(this, e._2))
    CPOutcome.Suspend
  }

  override def valBindIdx(x: CPVarInt, tank: Int) = {
    if (x.getValue == cargo.id) {
      curCapa.setValue(curCapa.getValue + tanks(tank).capa)
      if (curCapa.getValue >= cargo.volume) {
        // the volume is reached for the cargo so we prevent any other tank to take this cargo
        for (c <- cargos; if (!c.isBound)) {
          c.removeValue(cargo.id) // should never fail here
        }
        CPOutcome.Success
      } else {
        CPOutcome.Suspend
      }
    } else {
      CPOutcome.Suspend
    }
  }
}