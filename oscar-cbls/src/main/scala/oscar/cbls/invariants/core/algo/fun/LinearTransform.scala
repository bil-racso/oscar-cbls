package oscar.cbls.invariants.core.algo.fun

object LinearTransform{
  val identity = new LinearTransform(0,false)
  def apply(offset:Int,minus:Boolean) = new LinearTransform(offset,minus)
}

/**
 * linear transformer of position.
 * value => offset op value where op is + or minus, according to "boolean flip": true => - fase => +
 */
class LinearTransform(val offset:Int,val minus:Boolean){
  def apply(value:Int) = if(minus) offset - value else offset + value
  def unApply(value:Int) = if(minus) offset - value else value - offset

  /**
   * delivers a new linear transform that is equal to this(that(value))
   * @param that
   * @return
   */
  def apply(that: LinearTransform):LinearTransform = {
    new LinearTransform(this(that.offset),this.minus != that.minus)
  }

  def equals(that:LinearTransform):Boolean = {
    this.offset == that.offset && this.minus == that.minus
  }

  def isIdentity:Boolean = offset == 0 && !minus

  override def toString: String = (
    if(offset == 0) {
      if (minus) "(x=>-x)" else "(x=>x)"
    }else "(x=>" + offset + (if (minus) "-" else "+") + "x)")

  //this: if minus y = offset - x else y = offset + x
  //that : if minus x = offset - y else x = - offset + y
  def invert:LinearTransform = new LinearTransform(if(minus) offset else (-offset),minus)
}
