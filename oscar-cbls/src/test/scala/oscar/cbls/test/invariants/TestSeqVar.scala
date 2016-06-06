package oscar.cbls.test.invariants

import oscar.cbls.invariants.core.algo.seq.functional.{IntSequence, UniqueIntSequence}
import oscar.cbls.invariants.core.computation.{IntValue, SeqValue, Store, CBLSSeqVar}
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.seq.{PositionOf, Content, Size}

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = true, checker = Some(new ErrorChecker()))
  val a = new CBLSSeqVar(m,IntSequence(List(1,2,3,5)), n = "toto")

  val size1 = Size(a.createClone)
  val size2 = Size(a)
  val pos2 = PositionOf(a, 2, 1000)
  val content = Content(a)
  m.registerForPartialPropagation(size2)
  m.close()

  println(m.stats)

  println(size2)
//  println(content)

  println("insertAtPosition(45,3)")

  a.insertAtPosition(45,3)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + a.toStringNoPropagate)

  val checkpoint = a.defineCurrentValueAsCheckpoint(true)
  println("defined checkpoint " + checkpoint)
  println("insert&Move")

  a.move(1,3,4,false)
  a.insertAtPosition(12,5)
  a.remove(a.value.positionOfFirstOccurrence(2).head)
  a.move(1,3,4,true)
  a.move(1,3,4,true)

  println(size2)
  println(content)//fail ici
  println(a)
  println(pos2)

  require(5 == size2.value)

  println("\n\n")
  a.rollbackToCurrentCheckpoint(checkpoint)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + a.toStringNoPropagate)
  require(content.value equals SortedSet(1,2,3,5,45))
  println(content)
  require(a.value equals checkpoint)
  println(pos2)
  require(size1.value == size2.value)
}

