package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence

/**
 * Created by rdl on 20-04-16.
 */
object TestUniqueIntSequence extends App{
  val a = UniqueIntSequence()
  println(a)

  val b = a.insertAtPosition(5,0) //insert first
  println(b)
  require(b.positionOfValue(5).head == 0)

  val c = b.insertAtPosition(6,1)//insert at end
  println(c)
  require(c.positionOfValue(5).head == 0)
  require(c.positionOfValue(6).head == 1)
  require(c.valueAtPosition(0).head == 5)
  require(c.valueAtPosition(1).head == 6)

  val d = c.insertAtPosition(7,0)//insert at head
  println(d)
  require(d.positionOfValue(7).head == 0,"d.positionOfValue(7).head:" + d.positionOfValue(7).head)
  require(d.positionOfValue(5).head == 1)
  require(d.positionOfValue(6).head == 2)
  require(d.valueAtPosition(0).head == 7)
  require(d.valueAtPosition(1).head == 5)
  require(d.valueAtPosition(2).head == 6)


  val e = d.insertAtPosition(8,2)//insert inside
  println(e)
  require(e.positionOfValue(7).head == 0)
  require(e.positionOfValue(5).head == 1)
  require(e.positionOfValue(8).head == 2)
  require(e.positionOfValue(6).head == 3)
  require(e.valueAtPosition(0).head == 7)
  require(e.valueAtPosition(1).head == 5)
  require(e.valueAtPosition(2).head == 8)
  require(e.valueAtPosition(3).head == 6)



  val f = e.insertAtPosition(9,2)//insert inside
  println(f)
  require(f.positionOfValue(7).head == 0)
  require(f.positionOfValue(5).head == 1)
  require(f.positionOfValue(9).head == 2)
  require(f.positionOfValue(8).head == 3)
  require(f.positionOfValue(6).head == 4)
  require(f.valueAtPosition(0).head == 7)
  require(f.valueAtPosition(1).head == 5)
  require(f.valueAtPosition(2).head == 9)
  require(f.valueAtPosition(3).head == 8)
  require(f.valueAtPosition(4).head == 6)
}
