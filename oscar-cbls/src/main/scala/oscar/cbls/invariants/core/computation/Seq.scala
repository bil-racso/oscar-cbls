package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.seq.functional.{ConcreteUniqueIntSequence, UniqueIntSequence}

import scala.language.implicitConversions

sealed trait SeqValue extends Value{
  def value:UniqueIntSequence
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = value.toString
}

object SeqValue{
  implicit def tist2IntSeqVar(a:List[Int]):SeqValue = CBLSSeqConst(UniqueIntSequence(a))
}

sealed abstract class SeqUpdate(val newValue:UniqueIntSequence){
  /**
   * @return MAxInt is a load was done,
   */
  def nbUpdatesFromPreviousValue:Int
}

sealed abstract class SeqUpdateWithPrev(prev:SeqUpdate,newValue:UniqueIntSequence) extends SeqUpdate(newValue){
  def nbUpdatesFromPreviousValue:Int = {
    val tmp = prev.nbUpdatesFromPreviousValue
    if(tmp == Int.MaxValue) tmp else tmp+1
  }
}

//after is -1 for start position
case class SeqInsert(value:Int,pos:Int,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev:SeqUpdate, prev.newValue.insertAtPosition(value,pos,fast=true))

case class SeqMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true)){
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head
}

case class SeqRemoveValue(value:Int,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev,prev.newValue.delete(prev.newValue.positionOfValue(value).head,fast=true))

case class Set(val value:UniqueIntSequence) extends SeqUpdate(value){
  def nbUpdatesFromPreviousValue:Int = Int.MaxValue
}


trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean)
}

class CBLSSeqConst(override val value:ConcreteUniqueIntSequence) extends SeqValue{
  override def domain : Domain = value.largestValue match{case None => Domain.empty case Some(v) => 0 to v}
  override def name : String = value.toString
}

object CBLSSeqConst{
  implicit def seq2SeqValue(seq: UniqueIntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())
  implicit def seq2SeqConst(seq: UniqueIntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())

  def apply(seq:UniqueIntSequence):CBLSSeqConst = new CBLSSeqConst(seq.regularize())
}

abstract class SeqVar(initialValue:Iterable[Int], maxPivot:Int, maxValue:Int)
  extends ChangingSeqValue(initialValue, maxPivot, maxValue){

}

abstract class ChangingSeqValue(initialValue:Iterable[Int], maxPivot:Int, maxValue:Int)
  extends AbstractVariable with SeqValue{

  var cachedValue:UniqueIntSequence = UniqueIntSequence(initialValue,maxPivot,maxValue)
  var mOldValue:SeqUpdate = Set(cachedValue)
  var updates:SeqUpdate = mOldValue

  override def value: UniqueIntSequence = {
    if (model == null) return mOldValue.newValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mOldValue.newValue
    model.propagate(this)
    mOldValue.newValue
  }

  def newValue:UniqueIntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    updates.newValue
  }

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  //-1 for first position
  def insertAtPosition(value:Int,pos:Int){
    updates = SeqInsert(value,pos,updates)
  }

  def deleteValue(value:Int){
    updates = SeqRemoveValue(value,updates)
  }
  //-1 for first position
  def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    updates  = SeqMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,updates)
  }

  def set(seq:UniqueIntSequence){
      updates = Set(value)
  }

  final protected def performSeqPropagation(stableCheckpoint:Boolean): Unit = {
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next
    if(stableCheckpoint) cachedValue = updates.newValue //force computation for stable checkpoints
    while (currentElement != headPhantom) {
      val e = currentElement.elem
      currentElement = currentElement.next
      val inv : SeqNotificationTarget = e._1.asInstanceOf[SeqNotificationTarget]
      assert({
        this.model.NotifiedInvariant = inv.asInstanceOf[Invariant]; true
      })
      inv.notifySeqChanges(this, e._2, updates,stableCheckpoint)
      assert({
        this.model.NotifiedInvariant = null; true
      })
    }
    //perfoms the changes on the mNewValue
    //when it is a stable checkpoint, we save a cached value
    if(stableCheckpoint){
      val start = Set(updates.newValue.regularize())
      cachedValue = start.value
      mOldValue = start
      updates = start
    }else{
      //TODO
    }
  }
}
