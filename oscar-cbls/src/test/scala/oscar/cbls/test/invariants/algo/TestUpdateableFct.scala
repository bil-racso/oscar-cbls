package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.fun.mutable.{LinearPositionTransform, PiecewiseLinearFunction}
import oscar.cbls.invariants.core.algo.fun.functional.PiecewiseLinearFun

class UpdateableFunctionNaive(maxVal:Int) {
  //external position => internal position
  private var transformation: Array[Int] = Array.tabulate(maxVal+1)((a:Int) => a)

  override def toString: String = {
    transformation.mkString(",")
  }

  def apply(value: Int): Int = {
    transformation(value)
  }

  def update(fromIncluded: Int, toIncluded: Int, additionalF: LinearPositionTransform): Unit = {
    for(i <- fromIncluded to toIncluded){
      transformation(i) = additionalF(transformation(i))
    }
  }
}


/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */


object TestUpdateableFunction extends App{
  val maxVal = 100
  val fn = new PiecewiseLinearFunction()
  var fnFun = new PiecewiseLinearFun()
  val fn2 = new UpdateableFunctionNaive(maxVal)

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      //println("checking " + i + ": " + fn(i))
      if(fn(i) != fn2(i)) throw new Error("mismatch " + i +  " fn(i)" + fn(i) + " naiveFn(i):" + fn2(i))
      if(fnFun(i) != fn2(i)) throw new Error("mismatch " + i + " fnFun(i)" + fnFun(i) + " naiveFn(i):" + fn2(i))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearPositionTransform): Unit ={
    println("BEFORE:"+
    "\nnaive:" + fn2 +
    "\nmutable:" + fn +
    "\nfunctional" + fnFun + "\n")

    fn.update(fromIncluded,toIncluded,add)
    fn2.update(fromIncluded,toIncluded,add)
    fnFun = fnFun.update(fromIncluded,toIncluded,add)
    println("AFTER:"+
      "\nnaive:" + fn2 +
      "\nmutable:" + fn +
      "\nfunctional" + fnFun + "\n")
    compare()
  }

  println("init:" + fn)

  update(6, 20, new LinearPositionTransform(-3,true))
  println(fn)
  println("f(10) = " +fn(10))
  println


  update(7, 14, new LinearPositionTransform(3,false))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(16, 19, new LinearPositionTransform(5,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(6, 16, new LinearPositionTransform(7,true))
  println(fn)
  println("f(10) = " +fn(10))
  println


  update(7, 14, new LinearPositionTransform(-3,false))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(6, 20, new LinearPositionTransform(-3,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(2, 80, new LinearPositionTransform(-30,true))
  update(10, 94, new LinearPositionTransform(10,false))
  update(3, 70, new LinearPositionTransform(-7,true))

  update(6, 19, new LinearPositionTransform(-13,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(7, 19, new LinearPositionTransform(2,false))
  println(fn)
  println("f(10) = " +fn(10))
  println


  update(7, 16, new LinearPositionTransform(4,false))
  println(fn)
  println("f(10) = " +fn(10))
  println


  update(15, 16, new LinearPositionTransform(-6,false))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(16, 16, new LinearPositionTransform(-11,true))
  println(fn)
  println("f(10) = " +fn(10))
  println
}
