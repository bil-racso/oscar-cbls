package oscar.cbls.invariants.core.algo.fun

sealed abstract class PiecewiseLinearFunNaive{
  def apply(value:Int):Int
  def updateBefore(fromIncuded:Int,toIncluded:Int,update:LinearTransform):PiecewiseLinearFunNaive =
    new UpdatedPiecewiseLinearFunNaive(fromIncuded,toIncluded,update:LinearTransform,this)
}
case object IdentityNaive extends PiecewiseLinearFunNaive{
  override def apply(value:Int):Int = value
}
case class UpdatedPiecewiseLinearFunNaive(fromIncuded:Int,toIncluded:Int,update:LinearTransform,base:PiecewiseLinearFunNaive) extends PiecewiseLinearFunNaive{
  override def apply(value:Int):Int = if(value >= fromIncuded && value <= toIncluded) base(update(value)) else base(value)
}

