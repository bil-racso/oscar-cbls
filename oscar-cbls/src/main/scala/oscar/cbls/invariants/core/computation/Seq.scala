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
  protected[computation] def reverseTo(target:UniqueIntSequence):SeqUpdate = this.reverseAcc(target,SeqUpdateSet(newValue))
  protected[computation] def reverseAcc(target:UniqueIntSequence, newPrev:SeqUpdate):SeqUpdate
}

sealed abstract class SeqUpdateWithPrev(prev:SeqUpdate,newValue:UniqueIntSequence) extends SeqUpdate(newValue) {
  def oldPosToNewPos(oldPos:Int):Int
}

//after is -1 for start position
case class SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate)
                          (seq:UniqueIntSequence=prev.newValue.insertAtPosition(value,pos,fast=true))
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){
  assert(seq equals prev.newValue.insertAtPosition(value,pos,fast=true))
  override protected[computation] def reverseAcc(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverseAcc(target,SeqUpdateRemoveValue(value,newPrev)(prev.newValue))
  }
}

case class SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate)
                        (seq:UniqueIntSequence=prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))
  extends SeqUpdateWithPrev(prev,seq){
  assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head



  override protected[computation] def reverseAcc(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    //TODO: chek this!!!
    prev.reverseAcc(target,SeqUpdateMove(after - (toIncluded - fromIncluded), after, fromIncluded-1, flip, newPrev)(prev.newValue))
  }
}

case class SeqUpdateRemoveValue(value:Int,prev:SeqUpdate)
                               (seq:UniqueIntSequence=prev.newValue.delete(prev.newValue.positionOfValue(value).head,fast=true))
  extends SeqUpdateWithPrev(prev,seq){
  assert(seq equals prev.newValue.delete(prev.newValue.positionOfValue(value).head,fast=true))

  def position:Int = prev.newValue.positionOfValue(value).head

  override protected[computation] def reverseAcc(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverseAcc(target,SeqUpdateInsert(value, position, newPrev)(prev.newValue))
  }
}

case class SeqUpdateSet(value:UniqueIntSequence) extends SeqUpdate(value){
  override protected[computation] def reverseAcc(target : UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate =
    if(target quickEquals this.newValue) newPrev else SeqUpdateSet(target)
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

class CBLSSeqVar(givenModel:Store, initialValue:UniqueIntSequence, val maxVal:Int = Int.MaxValue, n: String = null, maxPivot:Int = 10)
  extends ChangingSeqValue(initialValue, maxVal, maxPivot) with Variable{
  require(domain.min == 0)
  require(givenModel != null)

  model = givenModel

  override def domain : Domain = 0 to maxVal

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

  override def setCheckpointStatus(willOftenRollBackToCurrentValue:Boolean) {super.setCheckpointStatus(willOftenRollBackToCurrentValue:Boolean)}

  override def rollbackToLatestCheckpoint(c:UniqueIntSequence,releaseCheckpoint:Boolean) =
    super.rollbackToLatestCheckpoint(c:UniqueIntSequence,releaseCheckpoint:Boolean)

  def <==(i: SeqValue) {IdentitySeq(this,i)}

  override def performPropagation(){performSeqPropagation()}
}

object CBLSSeqVar{
  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }
}

abstract class ChangingSeqValue(initialValue: Iterable[Int], maxValue: Int, maxPivot: Int)
  extends AbstractVariable with SeqValue{

  private var mOldValue:SeqUpdate = SeqUpdateSet(UniqueIntSequence(initialValue))
  private var updates:SeqUpdate = mOldValue

  override def value: UniqueIntSequence = {
    if (model == null) return mOldValue.newValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return updates.newValue
    model.propagate(this)
    mOldValue.newValue
  }

  def newValue:UniqueIntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    updates.newValue
  }

  override def toString:String = name + ":=" + (if(model.propagateOnToString) value else updates.newValue)

  def toStringNoPropagate: String = name + ":=" + updates.newValue.toString()

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  //-1 for first position
  protected def insertAtPosition(value:Int,pos:Int){
    assert(pos < updates.newValue.size)
    assert(pos >= 0)
    updates = SeqUpdateInsert(value,pos,updates)()
    notifyChanged()
  }

  protected def removeValue(value:Int){
    updates = SeqUpdateRemoveValue(value,updates)()
    notifyChanged()
  }

  //-1 for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    updates  = SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,updates)()
    notifyChanged()
  }

  protected def setValue(seq:UniqueIntSequence){
    updates = SeqUpdateSet(value)
    notifyChanged()
  }

  protected def :=(seq:UniqueIntSequence){
    setValue(seq)
    notifyChanged()
  }


  //stack does not include top checkpoint
  private var checkpointStack:List[(UniqueIntSequence,SeqUpdate,Boolean)] = List.empty

  protected def pushTopCheckpointToStackIfSome(){
    topCheckpoint match {
      case Some(c) =>
        checkpointStack = (c,committedSinceTopCheckpoint,willOftenRollBackToTopCheckpoint) :: checkpointStack
        topCheckpoint = None
      case None => ;
    }
  }

  protected def popTopCheckpointFromStack(){
    val (top :: tail) = checkpointStack
    checkpointStack = tail
    topCheckpoint = Some(top._1)
    committedSinceTopCheckpoint = top._2
    willOftenRollBackToTopCheckpoint = top._3
  }

  private var willOftenRollBackToTopCheckpoint:Boolean = false
  private var topCheckpoint:Option[UniqueIntSequence] = None
  private var committedSinceTopCheckpoint:SeqUpdate = null


  //immediately triggers a notification to listening invariants
  protected def setCheckpointStatus(willOftenRollBackToCurrentValue:Boolean){
    pushTopCheckpointToStackIfSome()
    this.willOftenRollBackToTopCheckpoint = willOftenRollBackToCurrentValue
    val v = this.newValue
    this.topCheckpoint = Some(v)
    this.committedSinceTopCheckpoint = SeqUpdateSet(v)
  }

  /**
   * rolls back to the latest checkpoint.
   * if c is not null, it checks that c is indeed the latest checkpoint
   * this will re-enable checkpoint if if was disabled, and if a checkpoint is recorded
   * @param c
   */
  protected def rollbackToLatestCheckpoint(c:UniqueIntSequence, releaseCheckpoint:Boolean){
    if(c != null) require(c quickEquals latestCheckpoint)
    //TODO: what if le checkpoint n'a pas encore été communiqué
    setValue(latestCheckpoint)
    notifyChanged()
  }

  /**
   *  checkpoints are managed in a stack fashion.
   *  you can basically set a checkpoint (= push)
   *  and release a checkpoint (=pop, and chack that head == specified checkpoint)
   *  a checkpoint is defined by the new value of the sequence hen it is defined.
   *
   *  a checkpoint enables the use of rollBack to latest checkpoint (which must be the latest defined one)
   *  you cannot rollback to an earlier checkpoint if you have not released a more recent one.
   *
   * upon propagation, invariants are notified if the new value is a stable checkpoint, or not.
   *
   * we consider that invariants have a single checkpoint.
   * when moving from one checkpoint to an older one, the invariant is therefore
   * notified about incremental updates to reach the older checkpoint
   * (these can actually be non-incremental as well,
   * if the search procedure used the "set" update at some point).
   *
   * this variable is therefore able to reverse a forward sequence of udpate.
   *
   * also, you can only release checkpoint in a stack fashion, and revert to the latestCheckpoint.
   */


  final protected def performSeqPropagation() = privatePerformSeqPropagation(false)

  private def privatePerformSeqPropagation(stableCheckpoint:Boolean ): Unit = {
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
      //TODO: probably the other way round
      val start = SeqUpdateSet(updates.newValue.regularize())
      latestCheckpoint = start.value
      mOldValue = start
      updates = start
    }else{
      mOldValue = SeqUpdateSet(updates.newValue)
      //TODO
    }
  }
}

/** this is a special case of invariant that has a single output variable, that is a Seq
  * @author renaud.delandtsheer@cetic.be
  */
abstract class SeqInvariant(initialValue:UniqueIntSequence, maxValue:Int = Int.MaxValue, maxPivot:Int = 10)
  extends ChangingSeqValue(initialValue, maxValue:Int, maxPivot)
  with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):SeqInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(){
    performInvariantPropagation()
    performSeqPropagation()
  }

  override def getDotNode:String = throw new Error("not implemented")
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
