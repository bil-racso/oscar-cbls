package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence

/**
 * Created by rdl on 20-04-16.
 */
object TestUniqueIntSequence extends App{
  val a = UniqueIntSequence()
  println(a)

  def check(seq:UniqueIntSequence,pos:Int,value:Int){
    require(seq.positionOfValue(value).head == pos,seq + ".positionOfValue(" + value + ").head:" + seq.positionOfValue(value).head + " expected:" + pos)
    require(seq.valueAtPosition(pos).head == value)
  }

  def checkSeq(seq:UniqueIntSequence,values:Int*){
    for((value,pos) <- values.zipWithIndex){
      check(seq,pos,value)
    }
    require(seq.iterator.toList equals values.toList)
    var crawlerAtEnd = seq.crawlerAtPosition(seq.size-1)
    var acc:List[Int] = List.empty
    while(crawlerAtEnd match{case Some(c) => acc = c.value :: acc; crawlerAtEnd = c.prev; true case None => false}){}
    require(acc equals values.toList, "acc:" + acc)

  }

  val b = a.insertAtPosition(5,0) //insert first
  println(b)
  checkSeq(b,5)

  val c = b.insertAtPosition(6,1)//insert at end
  println(c)
  checkSeq(c,5,6)

  val d = c.insertAtPosition(7,0)//insert at head
  println(d)
  checkSeq(d,7,5,6)

  val e = d.insertAtPosition(8,2)//insert inside
  println(e)
  checkSeq(e,7,5,8,6)

  val f = e.insertAtPosition(9,2)//insert inside
  println(f)
  checkSeq(f,7,5,9,8,6)
}
