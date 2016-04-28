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
    require(seq.explorerAtValue(value).head.value == value)
    require(seq.explorerAtValue(value).head.position == pos)
    require(seq.explorerAtPosition(pos).head.value == value)
    require(seq.explorerAtPosition(pos).head.position == pos)
  }

  def checkSeq(seq:UniqueIntSequence,values:Int*){
    seq.check
    for((value,pos) <- values.zipWithIndex){
      check(seq,pos,value)
    }
    require(seq.iterator.toList equals values.toList, "seq.iterator.toList:" + seq.iterator.toList + "!= values.toList:" + values.toList)
    var crawlerAtEnd = seq.explorerAtPosition(seq.size-1)
    var acc:List[Int] = List.empty
    while(crawlerAtEnd match{case Some(c) => acc = c.value :: acc; crawlerAtEnd = c.prev; true case None => false}){}
    require(acc equals values.toList, "acc:" + acc)

    require(seq.regularize.iterator.toList equals values.toList, "seq.regularize.iterator.toList:" + seq.regularize.iterator.toList + " != values.toList:" + values.toList)
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

  val f = e.delete(1).regularize//delete inside
  println(f)
  checkSeq(f,7,8,6)

  val g = f.insertAtPosition(5,2).regularize//insert inside
  println(g)
  checkSeq(g,7,8,5,6)

  val h = g.insertAtPosition(10,0)//insert inside
  println(h)
  checkSeq(h,10,7,8,5,6)

  val i = h.insertAtPosition(54,3).regularize//insert inside
  println(i)
  checkSeq(i,10,7,8,54,5,6)

  val j = i.delete(2)//insert inside
  println(j)
  checkSeq(j,10,7,54,5,6)

  //println("flip")
  val kk = j.moveAfter(1,2,0,true)//flip
  println(kk)
  checkSeq(kk,10,54,7,5,6)

  val k = j.moveAfter(1,2,4,false).regularize//move upwards
  println(k)
  checkSeq(k,10,5,6,7,54)

  val kkk = j.moveAfter(1,2,4,true)//move upwards with flip
  println(kkk)
  checkSeq(kkk,10,5,6,54,7)

  val l = kkk.moveAfter(0,2,3,true)//move upwards with flip
  println(l)
  checkSeq(l,54,6,5,10,7)

  val m = kkk.moveAfter(1,4,0,true)//flip
  println(m)
  checkSeq(m,10,7,54,6,5)

  val n = kkk.moveAfter(1,4,0,false).regularize//nop
  println(n)
  checkSeq(n,10,5,6,54,7)


  val nd = n.moveAfter(2,4,0,false)//move downwards no flip
  println(nd)
  checkSeq(nd,10,6,54,7,5)


  val ndf = n.moveAfter(2,4,0,true)//move downwards flip
  println(ndf)
  checkSeq(ndf,10,7,54,6,5)

}
