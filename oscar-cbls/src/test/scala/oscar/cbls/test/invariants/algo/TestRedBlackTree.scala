package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.rb.RedBlackTree

import scala.collection.immutable.SortedMap
import scala.util.Random

/**
 * Created by rdl on 24-05-16.
 */
object TestRedBlackTree extends App{

  def checkEqual(a:SortedMap[Int,Boolean],b:RedBlackTree[Boolean]){
    require(a.size == b.size)
    for((key,value) <- a){
      require(b.get(key).head == value)
    }
    for((key,value) <- b.content){
      require(a.get(key).head == value)
    }

    for(i <- 0 to 1000){
      require(a.get(i) equals b.get(i))
    }
  }


  var reference:SortedMap[Int,Boolean] = SortedMap.empty
  var sut = RedBlackTree.empty[Boolean]
  val random = new Random()

  for(it <- 0 to 10000){
    if(random.nextBoolean()){
      //insert something
      val key:Int = (random.nextDouble()*1000).toInt
      val value = random.nextBoolean()
      reference = reference + ((key,value))
      sut = sut.insert(key,value)
      println("step " + it + " inserting " + key)
    }else if (reference.size > 2){
      //remove something
      val key:Int = (reference.keys.toList.apply((random.nextDouble()*reference.size).toInt))
      reference = reference - (key)
      sut = sut.remove(key)
      println("step " + it + " removing " + key)
    }
    checkEqual(reference,sut)
    //create set from sorted

    checkEqual(reference,RedBlackTree.makeFromSorted(reference.toList.sortBy(_._1)))
  }

}
