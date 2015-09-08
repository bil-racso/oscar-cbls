package oscar.examples.cbls

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.invariants.core.propagation.{PropagationElement, SymmetriesFinder}
import oscar.cbls.invariants.lib.numeric.{Sum, Sum2}
import oscar.cbls.modeling.{Invariants, AlgebraTrait}


object SymmetryDetectionExample extends App with AlgebraTrait with Invariants {
  override def main(args: Array[String]): Unit = {
    implicit val store: Store = new Store()

    val vars:Array[CBLSIntVar] = Array.tabulate(5){i:Int => CBLSIntVar(store, i)}

    val i1 = vars(0) + vars(2)

    val i2 = vars(1) + vars(2)

    val i3 = (vars(3) + vars(4)) * vars(0)

    val i4 = (vars(3) + vars(4)) * vars(1)

    val inputs = List(i1, i2, i3, i4)

    val out = Sum(inputs)

    store.close(false)

    println(i1.asInstanceOf[PropagationElement].localSymmetries)

    val sF : SymmetriesFinder = new SymmetriesFinder(store)

    val syms = sF.getSymmetries(vars.map(_.asInstanceOf[PropagationElement]), out.asInstanceOf[PropagationElement])
  }

}
