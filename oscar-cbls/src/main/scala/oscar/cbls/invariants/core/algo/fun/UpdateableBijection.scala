package oscar.cbls.invariants.core.algo.fun

import oscar.cbls.invariants.core.algo.quick.QList


object PiecewiseLinearBijectionNaive{
  def identity:PiecewiseLinearBijectionNaive = new PiecewiseLinearBijectionNaive(PiecewiseLinearFun.identity)

  def computeInvertedPivots(prevPivot : Pivot, remainingPivots : List[Pivot], newPivots : QList[Pivot] = null) : QList[Pivot] = {
    remainingPivots match {
      case Nil => newPivots
      case p1 :: p2 :: tail =>
        val fun = p1.f
        val invert = fun.invert
        val newPivot = new PivotWithTo(fun(if (fun.minus) p2.fromValue - 1 else p1.fromValue), invert, fun(if (fun.minus) p1.fromValue else p2.fromValue - 1))
        computeInvertedPivots(p1, p2 :: tail, QList(newPivot, newPivots))
      case p1 :: tail =>
        val fun = p1.f
        require(!fun.minus)
        val invert = fun.invert
        QList(new PivotWithTo(fun(p1.fromValue), invert, fun(if (fun.minus) Int.MinValue else Int.MaxValue)), newPivots)
    }
  }

  def apply(forward:PiecewiseLinearFun, backward:PiecewiseLinearFun):PiecewiseLinearBijectionNaive = new PiecewiseLinearBijectionNaive(forward,backward)
  def apply(forward:PiecewiseLinearFun) = new PiecewiseLinearBijectionNaive(forward)
}

class PiecewiseLinearBijectionNaive(val forward:PiecewiseLinearFun, val backward:PiecewiseLinearFun){

  def this(forward:PiecewiseLinearFun)= {
    this(forward,PiecewiseLinearFun.createFromPivots(PiecewiseLinearBijectionNaive.computeInvertedPivots(null, forward.pivots, null)))
  }

  def invert:PiecewiseLinearBijectionNaive = new PiecewiseLinearBijectionNaive(backward,forward)

  def checkBijection(){
    var pivots = backward.pivots
    while(true){
      pivots match{
        case p1 :: p2 :: tail =>
          require(p1.asInstanceOf[PivotWithTo].toValue + 1 == p2.fromValue, "not a bijection; p1:" + p1 + " p2:" + p2)
          pivots = p2 :: tail
        case _ => return
      }
    }
  }

  override def toString : String = {
    "Bijection.forward: " + forward + "\n"+
      "Bijection.backward:" + backward + "\n"
  }

  def updateBefore(updates:(Int,Int,LinearTransform)*):PiecewiseLinearBijectionNaive ={
    var updatedForward = forward
    println("updating bijection with:" + updates)
    for((fromPos,toPos,update) <- updates){
      updatedForward = updatedForward.updateForCompositionBefore(fromPos,toPos,update)
      println("intermediary:" + updatedForward)
    }
    new PiecewiseLinearBijectionNaive(updatedForward)
  }

  def apply(value:Int) = forward(value)
  def unApply(value:Int) = backward(value)
}

class PivotWithTo(fromValue:Int,f:LinearTransform, val toValue:Int) extends Pivot(fromValue,f) {
  override def toString = "PivotWithTo(from:" + fromValue + " toValue:" + toValue + " " + f + ")"
}