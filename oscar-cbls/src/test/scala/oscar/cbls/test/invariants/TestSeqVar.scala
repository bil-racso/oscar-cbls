package oscar.cbls.test.invariants

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation.{Store, CBLSSeqVar}
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.seq.{Content, Size}

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = false, checker = Some(new ErrorChecker()))
  val a = new CBLSSeqVar(m,UniqueIntSequence(List(1,2,3,5)), n = "toto")

  val cloned = a.createClone
  val size2 = Size(cloned)
  val content = Content(a)
  //m.registerForPartialPropagation(size2)
  m.close()

  println(m.stats)

  m.propagate(size2)
  println(size2)
  m.propagate(content)
  println(content)

  println("insertAtPosition(45,3)")

  a.insertAtPosition(45,3)
  m.propagate(size2)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + cloned.toStringNoPropagate + a.toStringNoPropagate)
  m.propagate(content)
  require(content.value equals SortedSet(1,2,3,5,45))
  println(content)

  m.propagate(a)
  println(a)

  val checkpoint = a.defineCurrentValueAsCheckpoint(true)

  println("defined checkpoint " + checkpoint)
  println("insert&Move")


  a.move(1,3,4,true)
  a.insertAtPosition(12,5)

  m.propagate(size2)
  println(size2)
  m.propagate(content)
  println(content)
  m.propagate(a)
  println(a)

  m.propagate(size2)
  require(6 == size2.value)

  println("\n\n")
  a.rollbackToCurrentCheckpoint(checkpoint)
  m.propagate(size2)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + cloned.toStringNoPropagate + a.toStringNoPropagate)
  m.propagate(content)
  require(content.value equals SortedSet(1,2,3,5,45))
  println(content)

}
