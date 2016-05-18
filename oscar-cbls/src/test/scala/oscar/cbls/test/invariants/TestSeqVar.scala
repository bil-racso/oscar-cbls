package oscar.cbls.test.invariants

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation.{Store, CBLSSeqVar}
import oscar.cbls.invariants.lib.seq.Size

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = true)
  val a = new CBLSSeqVar(m,UniqueIntSequence(List(1,2,3,5)), n = "toto")
  val s = Size(a)

  m.close()

  println(m.stats)

  println(s)
  a.insertAtPosition(45,3)


  println(a)
  println(s)
}
