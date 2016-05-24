package oscar.cbls.test.invariants

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation.{Store, CBLSSeqVar}
import oscar.cbls.invariants.lib.seq.{Content, Size}

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = true)
  val a = new CBLSSeqVar(m,UniqueIntSequence(List(1,2,3,5)), n = "toto")
  val s = Size(a)


  val content = Content(a)
  m.close()

  println(m.stats)

  println(s)
  println(content)

  println("insertAtPosition(45,3)")

  a.insertAtPosition(45,3)
  require(s.value == 5)
  require(content.value equals SortedSet(1,2,3,5,45))
  println(content)
  println(a)

  println("insert&Move")

  a.move(1,3,4,true)
  a.insertAtPosition(12,5)

  println(s)
  println(content)
  println(a)

}
