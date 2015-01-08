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

import oscar.cbls.invariants.core.propagation.{PropagationElement, Checker}

import scala.collection.immutable.SortedSet



/**An IntSetVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is set of integer.
  * @param model is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSSetConst]]
  * @param MinVal is the minimum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays
  * @param MaxVal is the maximum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays.
  * @param Value is the value of the variable
  * @param name is the name of the variable, used for pretty printing only
  * @author renaud.delandtsheer@cetic.be
  * */
class CBLSSetVar(override val model:Store,
                 private val MinVal:Int,
                 private val MaxVal:Int,
                 override val name:String,
                 private var Value:SortedSet[Int]=SortedSet.empty)
  extends Variable(model,name){

  //bulk assign starts all
  //then, value -> add/delete
  //basically the bulk assign is the main issue here.
  //maybe invariants should specify to the var the kind of update they are interested in??

  def getMinVal:Int = MinVal
  def getMaxVal:Int = MaxVal
  def getModel = model

  private var ToPerform: List[(Int,Boolean)] = List.empty
  private var OldValue:SortedSet[Int] = Value

  private def Perform(){
    def Update(l:List[(Int,Boolean)]){
      if (l.isEmpty) return
      Update(l.tail)
      val (v, inserted) = l.head
      if (inserted) OldValue += v
      else OldValue -= v
    }
    Update(ToPerform)
    ToPerform = List.empty
  }

  override def checkInternals(c:Checker){
    assert(this.definingInvariant == null || OldValue.intersect(Value).size == Value.size,
      "internal error: " + "Value: " + Value + " OldValue: " + OldValue)
  }

  override def toString:String = name + ":={" + (if(model.propagateOnToString) value else Value).mkString(",") + "}"

  def valueString:String = "{" + value.mkString(",") + "}"


  /** this method is a toString that does not trigger a propagation.
    * use this when debugguing your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate: String = name + ":={" + Value.foldLeft("")(
    (acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"



  /**The values that have bee impacted since last propagation was performed.
    * null if set was assigned
    */
  var TouchedValues:List[(Int,Boolean)] = List.empty

  def insertValue(v:Int){
    if (!Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,true) :: TouchedValues
      Value +=v
      notifyChanged()
    }
  }

  def deleteValue(v:Int){
    if (Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,false) :: TouchedValues
      Value -=v
      notifyChanged()
    }
  }

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  def setValue(v:SortedSet[Int]){
    TouchedValues = null
    Value = v
    notifyChanged()
  }

  override def performPropagation(){
    if(getDynamicallyListeningElements.isEmpty){
      //no need to do it gradually
      OldValue=Value
    }else{
      if (TouchedValues == null){
        //need to calll every listening one, so gradual approach required
        OldValue.diff(Value).foreach(v => {
          OldValue -=v
          for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.getModel.NotifiedInvariant=inv; true})
            inv.notifyDeleteOnAny(this,e._2,v)
            assert({this.getModel.NotifiedInvariant=null; true})
          }
        })
        //puis on fait partir tous les insert
        Value.diff(OldValue).foreach(v => {
          OldValue += v
          for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.getModel.NotifiedInvariant=inv; true})
            inv.notifyInsertOnAny(this,e._2,v)
            assert({this.getModel.NotifiedInvariant=null; true})
          }
        })
        //puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
        assert(OldValue.intersect(Value).size == Value.size, "mismatch: OLD" + OldValue + " New:" + Value)
        OldValue=Value
      }else{
        //only touched values must be looked for
        for ((v,inserted) <- TouchedValues.reverse){
          //We simply replay the history. If some backtrack was performed, it is suboptimal
          // eg: if something was propagated to this during a neighbourhood exploration not involving this var
          if (inserted){
            //inserted
            ToPerform = (v, inserted) :: ToPerform
            for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
              val inv:Invariant = e._1.asInstanceOf[Invariant]
              assert({this.getModel.NotifiedInvariant=inv;true})
              inv.notifyInsertOnAny(this,e._2,v)
              assert({this.getModel.NotifiedInvariant=null;true})
            }
          }else{
            //deleted
            ToPerform = (v, inserted) :: ToPerform
            for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
              val inv:Invariant = e._1.asInstanceOf[Invariant]
              assert({this.getModel.NotifiedInvariant=inv;true})
              inv.notifyDeleteOnAny(this,e._2,v)
              assert({this.getModel.NotifiedInvariant=null;true})
            }
          }
        }
        OldValue = Value //in case we were lazy on the update
        ToPerform = List.empty
      }
    }
    TouchedValues = List.empty
  }

  def value:SortedSet[Int] = getValue(false)

  def getValue(NewValue:Boolean=false):SortedSet[Int] = {
    if (NewValue){
      assert(getModel.checkExecutingInvariantOK(definingInvariant),
        "variable [" + this + "] queried for latest val by non-controlling invariant")
      Value
    }else{
      if (model == null) return Value
      if (definingInvariant == null && !model.Propagating) return Value
      model.propagate(this)
      Perform()
      OldValue
    }
  }

  /**Use this to specify that the IntSetVar is the output of the IntSetInvariant*/
  def <==(i:SetInvariant){i.setOutputVar(this)}
  def <==(i: CBLSSetVar) {this <== IdentitySet(i)}

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  def :=(v:SortedSet[Int]) {setValue(v)}

  def :+=(i:Int) {this.insertValue(i)}
  def :-=(i:Int) {this.deleteValue(i)}

  def getDotNode = "[label = \"IntSetVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object CBLSSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  def apply(r:Range = Int.MinValue to Int.MaxValue, v:Iterable[Int] = List.empty, name:String="")(implicit s:Store) = {
    val emptySet:SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, r.start, r.end,name, emptySet ++ v)
  }

  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }

  implicit def intSet2IntSetVar(a:SortedSet[Int]):CBLSSetVar = CBLSSetConst(a)

  implicit def toFunction(s:CBLSSetVar):()=>SortedSet[Int] = () => s.value
}

/**
 * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param ConstValue: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 * */
case class CBLSSetConst(ConstValue:SortedSet[Int],override val model:Store = null)
  extends CBLSSetVar(model
    ,if(ConstValue.isEmpty) Int.MinValue else ConstValue.min
    ,if(ConstValue.isEmpty) Int.MaxValue else ConstValue.max
    ,toString,ConstValue){
  override def getValue(NewValue:Boolean=false):SortedSet[Int] = ConstValue //pour pas avoir de propagation
  override def toString:String = "IntSetConst{" + ConstValue.mkString(",") + "}"
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  * @param v
  */
case class IdentitySet(v:CBLSSetVar) extends SetInvariant{

  var output:CBLSSetVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  val myMin = v.getMinVal
  val myMax = v.getMaxVal

  override def checkInternals(c:Checker){
    c.check(output.getValue(true).intersect(v.value).size == v.value.size)
  }

  override def setOutputVar(vv:CBLSSetVar){
    output = vv
    output.setDefiningInvariant(this)
    output := v.value
  }

  override def notifyInsertOn(v:CBLSSetVar,value:Int){
    assert(v == this.v)
    output.insertValue(value)
  }

  override def notifyDeleteOn(v:CBLSSetVar,value:Int){
    assert(v == this.v)
    output.deleteValue(value)
  }
}


/*
* @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant extends Invariant{
  def myMin:Int
  def myMax:Int
  implicit def toSetVar:CBLSSetVar = {
    val a = new CBLSSetVar(model,myMin,myMax,this.getClass.getSimpleName,SortedSet.empty)
    a <== this //the variable calls setOutputVar
    a
  }

  def toSetVar(name:String):CBLSSetVar = {
    val a = new CBLSSetVar(model,myMin,myMax,name,SortedSet.empty)
    a <== this //the variable calls setoutputVar
    a
  }

  @deprecated("use toSetVar instead", "1.1")
  def toIntSetVar:CBLSSetVar = toSetVar

  /**this method is called by the output variable
    * basically, the invariant does not know what is its output variable at creation time.
    * if this is an issue, you can always create an output variable internally,
    * and implement this method with en identity invariant.
    * see [[oscar.cbls.invariants.core.computation.IdentityInt]] and [[oscar.cbls.invariants.core.computation.IdentitySet]]
    * @param v the variable that is the output variable of the invariant.
    */
  def setOutputVar(v:CBLSSetVar)
}

object SetInvariant{
  implicit def toIntSetVar(i:SetInvariant):CBLSSetVar = i.toSetVar
}


