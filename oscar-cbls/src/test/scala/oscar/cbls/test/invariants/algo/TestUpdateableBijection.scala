package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.fun.mutable.{LinearTransform$, PiecewiseLinearFun}
import oscar.cbls.invariants.core.algo.fun.functional.{PiecewiseLinearBijectionNaive, PiecewiseLinearFun}

import scala.collection.parallel.mutable


object TestUpdateableBijection extends App{
  val maxVal = 100
  val fn = new oscar.cbls.invariants.core.algo.fun.mutable.PiecewiseLinearFun()
  var fnFun = new oscar.cbls.invariants.core.algo.fun.functional.PiecewiseLinearFun()
  var reverse = new PiecewiseLinearBijectionNaive(fnFun)

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      //println("checking " + i + ": " + fn(i))
      if(fn(i) != fnFun(i)) throw new Error("mismatch " + i +  " fn(i)" + fn(i) + " naiveFn(i):" + fnFun(i))
      if(reverse.backward(fn(i)) != i)  throw new Error("mismatch on bijection i:" + i + " fnFun(i):" + fnFun(i) + " reverse.backward(fn(i)):" + reverse.backward(fn(i)))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearTransform): Unit ={
    println("BEFORE:"+
    "\nnaive:     " + fn +
    "\nfunctional:" + fnFun +
    "\nreverse:   " + reverse.backward + "\n")

    println("AFTER:")

    fn.update(fromIncluded,toIncluded,add)
    println("mutable:   " + fn)

    fnFun = fnFun.composeAfter(fromIncluded,toIncluded,add)
    println("functional:" + fnFun)

    reverse = new PiecewiseLinearBijectionNaive(fnFun)

    println("reverse:   " + reverse.backward + "\n")

    reverse.checkBijection()

    compare()
  }

  println("init:" + fn)

  update(6, 20, new LinearTransform(26,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(7, 14, new LinearTransform(31,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(22,40, new LinearTransform(62,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

}
