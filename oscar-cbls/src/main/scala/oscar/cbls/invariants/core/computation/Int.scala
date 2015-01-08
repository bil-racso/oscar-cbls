/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.propagation.{DummyPropagationElement, Checker, PropagationElement}

/** this is something that has an integer value.
  * this value can be queried, and invariants can be posted on it,
  * and it can be used on the righ hand of <== operator
  */
trait IntValue extends Value{
  def value: Int
  def domain:Domain
  def min = domain.min
  def max = domain.max
}

/**An IntVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is integer.
  *
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingIntValue(initialDomain:Domain, initialValue:Int)
  extends AbstractVariable with IntValue{

  private var privatedomain:Domain = initialDomain
  private var Value: Int = initialValue
  private var OldValue = Value

  def domain:Domain = privatedomain

  override def toString = if(model.propagateOnToString) s"$name:=$value" else defaultName
  override def toStringNoPropagate = s"$name:=$Value"

  def restrictDomain(d:Domain): Unit ={
    privatedomain = privatedomain.restrict(d)
  }


  def setValue(v:Int){
    if (v != Value){
      //TODO: disable assert while domain of invariant are buggy, this assert is needed in UNIT TEST.
      // (-Xdisable-assertions as argument of scala compiler)
      // or comment this assert and use it only to throw unit test while domain bugs.
      /*
      assert(inDomain(v),print("Assertion False : variable ["+this+"] is not in his domain \n" +
          "domain : ["+MinVal+ ";"+MaxVal+"]\n" +
           "new value :"+ v +"\n" ))
           */
      Value = v
      notifyChanged()
    }
  }

  override def value: Int = getValue()

  def getValue(NewValue: Boolean = false): Int = {
    if(NewValue){
      assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
        + "] queried for latest val by non-controlling invariant")
      Value
    } else{
      if (model == null) return Value
      if (definingInvariant == null && !model.Propagating) return Value
      model.propagate(this)
      OldValue
    }
  }

  override def performPropagation(){
    if(OldValue!=Value){
      val old=OldValue
      OldValue=Value
      for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){ //TODO: here should come some postponed stuff as well
      val inv:Invariant = e._1.asInstanceOf[Invariant]
        assert({this.model.NotifiedInvariant=inv; true})
        inv.notifyIntChangedAny(this,e._2,old,Value)
        assert({this.model.NotifiedInvariant=null; true})
      }
    }
  }

  override def checkInternals(c:Checker){
    c.check(OldValue == Value)
  }

  protected def :=(v: Int) {
    setValue(v)
  }

  protected def :+=(v: Int) {
    setValue(v + getValue(true))
  }

  protected def :*=(v: Int) {
    setValue(v * getValue(true))
  }

  protected def :-=(v:Int) {
    setValue(getValue(true) - v)
  }

  /** increments the variable by one
    */
  protected def ++ {
    setValue(1 + getValue(true))
  }

  def getDotNode = "[label = \"IntVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object ChangingIntValue{
  implicit val ord:Ordering[ChangingIntValue] = new Ordering[ChangingIntValue]{
    def compare(o1: ChangingIntValue, o2: ChangingIntValue) = o1.compare(o2)
  }
}



/**An IntVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSIntConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  */
class CBLSIntVar(givenModel: Store, initialDomain:Domain, initialValue: Int, n: String = null)
  extends ChangingIntValue(initialDomain,initialValue) with Variable{

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  override def :=(v: Int) {
    setValue(v)
  }

  override def :+=(v: Int) {
    setValue(v + getValue(true))
  }

  override def :*=(v: Int) {
    setValue(v * getValue(true))
  }

  override def :-=(v:Int) {
    setValue(getValue(true) - v)
  }

  /** increments the variable by one
    */
  override def ++ {
    setValue(1 + getValue(true))
  }

  /**this operator swaps the value of two IntVar*/
  def :=:(v:CBLSIntVar){
    val a:Int = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two IntVar*/
  def swap(v: CBLSIntVar) {
    this :=: v
  }

  def <==(i: IntValue) {IdentityInt(this,i)}
}

object CBLSIntVar{

  def apply(r:Range, v:Int, name:String)(implicit s:Store) = new CBLSIntVar(s,r,v,name)
  def apply(r:Range, v:Int)(implicit s:Store) = new CBLSIntVar(s,r,v,"")

  def apply(model: Store, minVal:Int, maxVal:Int, value:Int , name:String) = {
    require(minVal <= maxVal, "the minVal must be less than or equal to the maxVal of the domain minVal:" + minVal + " maxVal:" + maxVal)
    new CBLSIntVar(model,(minVal to maxVal), value, name)
  }

  def apply(model: Store, domain: Range, value:Int, name:String) = {
    require(!domain.isEmpty, "the domain supplied must be a valid increasing interval")
    new CBLSIntVar(model, domain, value, name)
  }

  def apply(model: Store, value:Int = 0, name:String = "") = {
    val domain = Int.MinValue to Int.MaxValue
    new CBLSIntVar(model, domain, value, name)
  }

  implicit val ord:Ordering[CBLSIntVar] = new Ordering[CBLSIntVar]{
    def compare(o1: CBLSIntVar, o2: CBLSIntVar) = o1.compare(o2)
  }

  implicit def int2IntValue(a:Int):IntValue = CBLSIntConst(a)

  implicit def toFunction(i:CBLSIntVar):()=>Int = () => i.value
}

/**
 * An IntConst is an [[oscar.cbls.invariants.core.computation.CBLSIntVar]] that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * notice that you should not attempt to create a CBLSIntConst directly; use the companion object for an efficient memoïzation
 * @param value: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 */
class CBLSIntConst(override val value:Int)
  extends IntValue{
  override def toString:String = "" + value
  override def domain: SingleValueDomain = new SingleValueDomain(value)
  override def min: Int = value
  override def max: Int = value
}

object CBLSIntConst{
  //I'v experimented with memoïzation in this class.
  //this is however not a so good idea because the memory itself takes some room, and tome time
  def apply(value:Int):CBLSIntConst = {
    new CBLSIntConst(value)
  }
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class IntInvariant(initialDomain:Domain = FullRange, initialValue:Int = 0)
  extends ChangingIntValue(initialDomain:Domain, initialValue:Int) with Invariant{

  override def definingInvariant: Invariant = this

  override def name: String = toString
}


object IdentityInt{
  def apply(v:IntValue):IntInvariant = new FullIdentityInt(v)
  def apply(toValue:CBLSIntVar, fromValue:IntValue){
    fromValue match{
      case c:CBLSIntConst => toValue := c.value
      case c:ChangingIntValue => IdentityInt(toValue, c)
    }
  }
}
/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  * @param v
  */
class FullIdentityInt(v:IntValue) extends IntInvariant(v.domain,v.value) {
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v:ChangingIntValue,i:Int,OldVal:Int,NewVal:Int){
    assert(v == this.v)
    this := NewVal
  }

  override def checkInternals(c:Checker){
    c.check(getValue(true) == v.value)
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityInt(toValue:CBLSIntVar, fromValue:ChangingIntValue) extends Invariant{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  override def notifyIntChanged(v:ChangingIntValue,OldVal:Int,NewVal:Int){
    toValue := NewVal
  }

  override def checkInternals(c:Checker){
    c.check(toValue.getValue(true) == fromValue.value)
  }
}
