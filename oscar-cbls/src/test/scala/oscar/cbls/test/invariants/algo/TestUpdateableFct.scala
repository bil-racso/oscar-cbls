package oscar.cbls.test.invariants.algo

import oscar.cbls.invariants.core.algo.fun.{PiecewiseLinearFun, LinearTransform}

sealed abstract class UpdateableFunctionNaive{
  def apply(value:Int):Int
  def updateBefore(fromIncuded:Int,toIncluded:Int,update:LinearTransform):UpdateableFunctionNaive =
    new UpdatedFunctionNaive(fromIncuded,toIncluded,update:LinearTransform,this)
}
case object IdentityNaive extends UpdateableFunctionNaive{
  override def apply(value:Int):Int = value
}
case class UpdatedFunctionNaive(fromIncuded:Int,toIncluded:Int,update:LinearTransform,base:UpdateableFunctionNaive) extends UpdateableFunctionNaive{
  override def apply(value:Int):Int = if(value >= fromIncuded && value <= toIncluded) base(update(value)) else base(value)
}



/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */


object TestUpdateableFunction extends App{
  val maxVal = 100
  var fnFun = new PiecewiseLinearFun()
  var fnNaive:UpdateableFunctionNaive = IdentityNaive

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      //println("checking " + i + ": " + fn(i))
      if(fnFun(i) != fnNaive(i)) throw new Error("mismatch " + i +  " fn(i)" + fnNaive(i) + " naiveFn(i):" + fnFun(i))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearTransform): Unit ={
    println("BEFORE:"+
    "\nnaive:     " + fnNaive +
    "\nfunctional:" + fnFun + "\n")

    println("UPDATE: fromIncluded:" + fromIncluded + " toIncluded:" + toIncluded + " add:" + add)

    fnNaive = fnNaive.updateBefore(fromIncluded,toIncluded,add)
    println("AFTER:"+
      "\nnaive:     " + fnNaive)

    fnFun = fnFun.updateForCompositionBefore(fromIncluded,toIncluded,add)
    println("functional:" + fnFun)

    compare()
  }

  println("init:" + fnFun)

  update(6, 20, new LinearTransform(26,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(7, 14, new LinearTransform(31,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println
  update(6, 20, new LinearTransform(-3,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println



  update(16, 19, new LinearTransform(5,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(6, 16, new LinearTransform(7,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(7, 14, new LinearTransform(-3,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(6, 20, new LinearTransform(-3,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(2, 80, new LinearTransform(-30,true))
  update(10, 94, new LinearTransform(10,false))
  update(3, 70, new LinearTransform(-7,true))

  update(6, 19, new LinearTransform(-13,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(7, 19, new LinearTransform(2,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(7, 16, new LinearTransform(4,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(15, 16, new LinearTransform(-6,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(16, 16, new LinearTransform(-11,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println
}
