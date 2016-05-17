package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.seq.functional.{StackedUpdateUniqueIntSequence, InsertedUniqueIntSequence, ConcreteUniqueIntSequence, UniqueIntSequence}
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet
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
case class SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev:SeqUpdate, prev.newValue.insertAtPosition(value,pos,fast=true)){
}

case class SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true)){
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head
}

case class SeqUpdateRemoveValue(value:Int,prev:SeqUpdate)
  extends SeqUpdateWithPrev(prev,prev.newValue.delete(prev.newValue.positionOfValue(value).head,fast=true)){
  def position:Int = prev.newValue.positionOfValue(value).head
}

case class SeqUpdateSet(val value:UniqueIntSequence) extends SeqUpdate(value){
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

class CBLSSeqVar(givenModel:Store, initialValue:UniqueIntSequence, val domain:Domain, n: String = null, maxPivot:Int = 10)
  extends ChangingSeqValue(initialValue, maxPivot, domain.max) with Variable{
  require(domain.min == 0)
  require(givenModel != null)

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  //-1 for first position
  override def insertAtPosition(value:Int,pos:Int){
    super.insertAtPosition(value,pos)
  }

  override  def removeValue(value:Int){
    super.removeValue(value)
  }

  //-1 for first position
  override  def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    super.move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean)
  }

  override  def setValue(seq:UniqueIntSequence) {super.setValue(seq)}

  override  def :=(seq:UniqueIntSequence) {super.setValue(seq)}

  def <==(i: SeqValue) {IdentitySeq(this,i)}
}


object CBLSSeqVar{
  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }
}

abstract class ChangingSeqValue(initialValue:Iterable[Int], maxPivot:Int, maxValue:Int)
  extends AbstractVariable with SeqValue{

  var latestCheckpoint:UniqueIntSequence = UniqueIntSequence(initialValue,maxPivot,maxValue)
  var mOldValue:SeqUpdate = SeqUpdateSet(latestCheckpoint)
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

  override def toString:String = name + ":={" + (if(model.propagateOnToString) value else updates.newValue).mkString(",") + "}"

  def toStringNoPropagate: String = name + ":=" + updates.newValue.toString()

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  //-1 for first position
  protected def insertAtPosition(value:Int,pos:Int){
    updates = SeqUpdateInsert(value,pos,updates)
  }

  protected def removeValue(value:Int){
    updates = SeqUpdateRemoveValue(value,updates)
  }
  //-1 for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    updates  = SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,updates)
  }

  protected def setValue(seq:UniqueIntSequence){
    updates = SeqUpdateSet(value)
  }

  protected def :=(seq:UniqueIntSequence){
    setValue(seq)
  }

  def setAsStableCheckpoint(){
    latestCheckpoint = newValue
  }

  def rollbackToLatestCheckpoint():UniqueIntSequence = {
    setValue(latestCheckpoint)
    latestCheckpoint
  }

  final protected def performSeqPropagation(stableCheckpoint:Boolean): Unit = {
    //TODO: manage the stableCheckpoint!!
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next
    if(stableCheckpoint) latestCheckpoint = updates.newValue //force computation for stable checkpoints
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
      val start = SeqUpdateSet(updates.newValue.regularize())
      latestCheckpoint = start.value
      mOldValue = start
      updates = start
    }else{
      //TODO
    }
  }
}

object IdentitySeq{
  def apply(toValue:CBLSSeqVar, fromValue:SeqValue){
    fromValue match{
      case c:CBLSSeqConst => toValue := c.value
      case c:ChangingSeqValue => new IdentitySeq(toValue, c)
    }
  }
}

class IdentitySeq(toValue:CBLSSeqVar, fromValue:ChangingSeqValue)
  extends Invariant
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate, stableCheckpoint : Boolean){
    assert(v == fromValue)
    digestChanges(changes)
  }

  def digestChanges(changes:SeqUpdate){
    changes match{
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.insertAtPosition(value,pos)
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.move(fromIncluded,toIncluded,after,flip)
      case SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.removeValue(value)
      case SeqUpdateSet(s) =>
        toValue.setValue(s)
    }
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value equals fromValue.value)
  }
}
