package oscar.cbls.invariants.core.algo.fun.functional

import oscar.cbls.invariants.core.algo.fun.mutable.LinearTransform$
import oscar.cbls.invariants.core.algo.quick.QList


object PiecewiseLinearBijectionNaive{
  def identity:PiecewiseLinearBijectionNaive = PiecewiseLinearBijectionNaive(PiecewiseLinearFun.identity)

  def apply(forward:PiecewiseLinearFun):PiecewiseLinearBijectionNaive = {
    new PiecewiseLinearBijectionNaive(forward,PiecewiseLinearFun.createFromPivots(computeInvertedPivots(null, forward.pivots, null)))
  }
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
}

class PiecewiseLinearBijectionNaive(val forward:PiecewiseLinearFun, val backward:PiecewiseLinearFun){

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

  def updateAfter(updates:(Int,Int,LinearTransform)*):PiecewiseLinearBijectionNaive ={
    var updatedForward = forward
    println("updating bijection with:" + updates)
    for((fromPos,toPos,update) <- updates){
      updatedForward = updatedForward.composeAfter(fromPos,toPos,update)
      println("intermediary:" + updatedForward)
    }
    PiecewiseLinearBijectionNaive(updatedForward)
  }

  def apply(value:Int) = forward(value)
  def unApply(value:Int) = backward(value)
}

class PivotWithTo(fromValue:Int,f:LinearTransform, val toValue:Int) extends Pivot(fromValue,f) {
  override def toString = "PivotWithTo(from:" + fromValue + " toValue:" + toValue + " " + f + ")"
}