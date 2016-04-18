package oscar.cbls.invariants.core.algo.fun.functional

import oscar.cbls.invariants.core.algo.fun.mutable.LinearPositionTransform
import oscar.cbls.invariants.core.algo.quick.QList

/**
 * Created by rdl on 15-04-16.
 */
class PiecewiseLinearBijectionNaive(val forward:PiecewiseLinearFun) {
  val backward:PiecewiseLinearFun = PiecewiseLinearFun.createFromPivots(computeInvertedPivots(null, forward.pivots, null))

  def apply(value:Int) = forward(value)
  def unApply(value:Int) = backward(value)

  private def computeInvertedPivots(prevPivot:Pivot, remainingPivots:List[Pivot], newPivots:QList[Pivot] = null):QList[Pivot] = {
    remainingPivots match{
      case Nil => newPivots
      case p1::p2::tail =>
        val fun = p1.f
        val invert = fun.invert
        val newPivot = new PivotWithTo(fun(if(fun.minus) p2.fromValue -1 else p1.fromValue), invert, fun(if(fun.minus) p1.fromValue else p2.fromValue -1))
        computeInvertedPivots(p1, p2::tail, QList(newPivot,newPivots))
      case p1::tail =>
        val fun = p1.f
        require(!fun.minus)
        val invert = fun.invert
        QList(new PivotWithTo(fun(p1.fromValue),invert,fun(if(fun.minus) Int.MinValue else Int.MaxValue)),newPivots)
    }
  }

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
}

class PivotWithTo(fromValue:Int,f:LinearPositionTransform, val toValue:Int) extends Pivot(fromValue,f) {
  override def toString = "PivotWithTo(from:" + fromValue + " toValue:" + toValue + " " + f + ")"
}