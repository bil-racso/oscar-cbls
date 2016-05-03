package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence

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

sealed abstract class SeqUpdate{
  def valueAfterThisUpdate:UniqueIntSequence
  def positionOfValueNew(value:Int):Option[Int]
  def valueAtPositionNew(pos:Int):Option[Int] = valueAfterThisUpdate.valueAtPosition(pos) //TODO improve this
  def containsNew(value:Int):Boolean
  def sizeNew:Int
  def nbUpdatesBeforeAvailable:Int

  /**
   * @return MAxInt is a load was done,
   */
  def nbUpdatesFromPreviousValue:Int
}

sealed abstract class SeqUpdateWithPrev(prev:SeqUpdate) extends SeqUpdate{
  override def nbUpdatesBeforeAvailable:Int = if(mySeqAfter == null) prev.nbUpdatesBeforeAvailable+1 else 0
  protected var mySeqAfter:UniqueIntSequence = null

  final override def valueAfterThisUpdate:UniqueIntSequence = {
    if(mySeqAfter == null) mySeqAfter = computeValueAfterThisUpdate()
    mySeqAfter
  }

  def computeValueAfterThisUpdate():UniqueIntSequence

  def valueBeforeThisUpdate:UniqueIntSequence = prev.valueAfterThisUpdate

  def nbUpdatesFromPreviousValue:Int = {
    val tmp = prev.nbUpdatesFromPreviousValue
    if(tmp == Int.MaxValue) tmp else tmp+1
  }
}

//after is -1 for start position
case class SeqInsert(value:Int,pos:Int,prev:SeqUpdate) extends SeqUpdateWithPrev(prev:SeqUpdate) {

  override def computeValueAfterThisUpdate:UniqueIntSequence = valueBeforeThisUpdate.insertAtPosition(value,pos)

  def positionOfValueNew(value:Int):Option[Int] = {
    if(mySeqAfter != null) return mySeqAfter.positionOfValue(value)
    if(value == this.value) Some(pos)
    else prev.positionOfValueNew(value) match{
      case None => None
      case Some(p) => if (p < pos) Some(p) else Some(p+1)
    }
  }

  def containsNew(value:Int):Boolean = {
    if(mySeqAfter != null) mySeqAfter.contains(value)
    else value == this.value || prev.containsNew(value)
  }

  def sizeNew:Int =
    if(mySeqAfter != null) mySeqAfter.size
    else prev.sizeNew +1
}

case class SeqMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) extends SeqUpdateWithPrev(prev:SeqUpdate){
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip

  override def computeValueAfterThisUpdate() : UniqueIntSequence = if(isNop) valueBeforeThisUpdate else valueBeforeThisUpdate.moveAfter(fromIncluded,toIncluded,after,flip)

  override def positionOfValueNew(value:Int):Option[Int] = {
    if(mySeqAfter != null) mySeqAfter.positionOfValue(value)
    else valueAfterThisUpdate.positionOfValue(value) //TODO improve this
  }

  def containsNew(value:Int):Boolean = {
    if(mySeqAfter != null) mySeqAfter.contains(value)
    else prev.containsNew(value)
  }

  def sizeNew:Int = {
    if (mySeqAfter != null) mySeqAfter.size
    else prev.sizeNew
  }
}

case class SeqRemoveValue(value:Int,prev:SeqUpdate) extends SeqUpdateWithPrev(prev:SeqUpdate) {

  override def computeValueAfterThisUpdate() : UniqueIntSequence = {
    val p = valueBeforeThisUpdate
    p.delete(p.positionOfValue(value).head)
  }

  def positionOfValueNew(value:Int):Option[Int] = {
    if(mySeqAfter != null) mySeqAfter.positionOfValue(value)
    else if(value == this.value) None
    else prev.positionOfValueNew(value) match{
      case None => None
      case Some(p) => if (p < prev.positionOfValueNew(value).head) Some(p) else Some(p-1)
    }
  }

  def containsNew(value:Int):Boolean = {
    if(mySeqAfter != null) mySeqAfter.contains(value)
    else prev.containsNew(value)
  }

  def sizeNew:Int =
    if(mySeqAfter != null) mySeqAfter.size
    else prev.sizeNew -1
}

sealed abstract class SeqUpdateNoPrev(val value:UniqueIntSequence) extends SeqUpdate{
  override def nbUpdatesBeforeAvailable:Int = 0
  override def valueAfterThisUpdate:UniqueIntSequence = value
  override def positionOfValueNew(value:Int):Option[Int] = this.value.positionOfValue(value)
  override def valueAtPositionNew(pos:Int):Option[Int] = value.valueAtPosition(pos)
  override def containsNew(value:Int):Boolean = this.value.contains(value)
  override def sizeNew:Int = this.value.size
}

case class SetORRestore(override val value:UniqueIntSequence, val isRestore:Boolean) extends SeqUpdateNoPrev(value){
  def nbUpdatesFromPreviousValue:Int = Int.MaxValue
}
case class StartingPoint(override val value:UniqueIntSequence) extends SeqUpdateNoPrev(value){
  def nbUpdatesFromPreviousValue:Int = 0
}

trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean)
}

class CBLSSeqConst(override val value:UniqueIntSequence) extends SeqValue{
  override def domain : Domain = value.largestValue match{case None => Domain.empty case Some(v) => 0 to v}
  override def name : String = value.toString
}

object CBLSSeqConst{
  implicit def seq2SeqValue(seq: UniqueIntSequence): CBLSSeqConst = new CBLSSeqConst(seq)
  implicit def seq2SeqConst(seq: UniqueIntSequence): CBLSSeqConst = new CBLSSeqConst(seq)

  def apply(seq:UniqueIntSequence):CBLSSeqConst = new CBLSSeqConst(seq)
}

abstract class ChangingSeqValue(initialValue:Iterable[Int], maxPivot:Int, maxValue:Int)
  extends AbstractVariable with SeqValue{

  var cachedValue:UniqueIntSequence = UniqueIntSequence(initialValue,maxPivot,maxValue)
  var mOldValue:SeqUpdate = StartingPoint(cachedValue)
  var updates:SeqUpdate = mOldValue

  override def value: UniqueIntSequence = {
    if (model == null) return mOldValue.valueAfterThisUpdate
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mOldValue.valueAfterThisUpdate
    model.propagate(this)
    mOldValue.valueAfterThisUpdate
  }

  def newValue:UniqueIntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    updates.valueAfterThisUpdate
  }

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  //-1 for first position
  def insertAtPosition(value:Int,pos:Int){
    updates = SeqInsert(value,pos,updates)
  }
  def deleteValue(value:Int){
    updates = SeqRemoveValue(value,updates )
  }
  //-1 for first position
  def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    updates  = SeqMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,updates)
  }
  def setORRestore(seq:UniqueIntSequence){
      updates = SetORRestore(value,(value == cachedValue))
  }

  final protected def performSeqPropagation(stableCheckpoint:Boolean): Unit = {
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next
    if(stableCheckpoint) cachedValue = updates.valueAfterThisUpdate //force computation for stable checkpoints
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
      val start = StartingPoint(updates.valueAfterThisUpdate)
      cachedValue = start.value
      mOldValue = start
      updates = start
    }else{
      //TODO
    }
  }
}


