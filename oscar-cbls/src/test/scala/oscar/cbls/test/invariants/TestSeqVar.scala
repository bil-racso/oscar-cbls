package oscar.cbls.test.invariants

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation.{Store, CBLSSeqVar}

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = true)
  val a = new CBLSSeqVar(m,UniqueIntSequence(List(1,2,3,5)), n = "toto")

  m.close()

  a.insertAtPosition(45,3)

  println(a)
}
