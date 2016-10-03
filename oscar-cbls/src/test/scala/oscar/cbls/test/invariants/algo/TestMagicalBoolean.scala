package oscar.cbls.test.invariants.algo

import oscar.cbls.algo.magicalBoolean.MagicalBoolean
import scala.util.Random

/**
  * Created by  Jannou Brohée on 3/10/16.
  */
object TestMagicalBoolean extends App{

  def checkEqual(a:Array[Boolean],b:MagicalBoolean) {
    require(a.length == b.size)
    for (id <- 0 until a.length - 1) {
      require(a(id) == b.get(id))
    }
  }
  var reference: Array[Boolean] =  Array.ofDim(10000)
  require( MagicalBoolean(-10000)==null)
  var sut:MagicalBoolean  = MagicalBoolean(10000)
  val random = new Random()

  for(it <- 0 to 9999){
    val value = random.nextBoolean()
    reference(it)=value
    sut.set(it,value)
  }
  var iter= sut.iterator
  var iterString :String ="["
  var iter2List = iter.toList
  iter= sut.iterator
  while(iter.hasNext){
    val tmp:Int = iter.next()
    iterString+= tmp.toString
    if(iter.hasNext) iterString+=","
    require(sut.get(tmp))
  }
  iterString+="]"
  //check toString()
  require(sut.toString.equals(iterString))

  //check if there are elements  "true" which is not in the iterator
  for(n<- 0 until sut.length-1){
    if(!iter2List.contains(n)) require(!sut.get(n))
  }

  checkEqual(reference,sut)
  var reference2: Array[Boolean] =  Array[Boolean](false,false,false,false,false,false,false,false,false,false)
  var reference3: Array[Boolean] =  Array[Boolean](true,true,true,true,true,true,true,true,true,true)
  sut  = MagicalBoolean(10)
  var tim:Long = System.currentTimeMillis()
 /* for(it <-0 to Int.MaxValue-2){
    val value = random.nextBoolean()
   // if(verbose) println("set element at id :"+0+" to value :"+value)
    sut.set(0,value)
    //if(verbose) println("clearAll()")
    sut.clearAll()//==> false
    checkEqual(reference2,sut)
    //if(verbose)  println("setAll()")
    sut.setAll() //==> true
    checkEqual(reference3,sut)
  }
  require(sut.global== -1)
  println((Int.MaxValue-2)+" iterations ==>  "+(System.currentTimeMillis()-tim)+"ms")*/


}
