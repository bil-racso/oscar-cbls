package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.mutable.{ImmutableIntSequence, UniqueIntSequenceWithPosition, IntSequence}
//import oscar.cbls.invariants.core.algo.seq.functional.ImmutableIntSequence

sealed trait SeqValue extends Value{
  def value:IntSequence
  def immutableValue:ImmutableIntSequence
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = "{" + value.mkString(",") + "}"
}

object SeqValue{
  //  implicit def tist2IntSeqVar(a:List[Int]):SeqValue = CBLSSeqConst(a)
}

sealed abstract class SeqUpdate
class SeqInsert(value:Int,after:Int) extends SeqUpdate//after is -1 for start position
class SeqMove(from:Int,to:Int,after:Int,flip:Boolean) extends SeqUpdate
class SeqRemove(value:Int) extends SeqUpdate
class SeqSet(value:QList[Int]) extends SeqUpdate

trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, change:SeqUpdate, oldValue: IntSequence, newValue: IntSequence)
}

abstract class ChangingSeqValue(initialValue:Iterable[Int], maxValue:Int)
  extends AbstractVariable with SeqValue{

  val mNewValue:IntSequence = new UniqueIntSequenceWithPosition(maxValue)
  val mOldValue:IntSequence = new UniqueIntSequenceWithPosition(maxValue)

  override def value: IntSequence = {
    if (model == null) return mNewValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mNewValue
    model.propagate(this)
    mOldValue
  }

  def newValue:IntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    mNewValue
  }

  var updates:QList[SeqUpdate] = null

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  def insertBeforeValue(value:Int,beforeValue:Int) //-1 for last position
  def insertAfterValue(value:Int,afterValue:Int) //-1 for first position
  def delete(value:Int)
  def move(fromIncludedValue:Int,toIncludedValue:Int,afterValue:Int,flip:Boolean) //-1 for first position
  def set(seq:Iterable[Int])

  final protected def performSeqPropagation(): Unit = {
    if(updates != null) for(update <- updates.reverse) {
      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while (currentElement != headPhantom) {
        val e = currentElement.elem
        currentElement = currentElement.next
        val inv : SeqNotificationTarget = e._1.asInstanceOf[SeqNotificationTarget]
        assert({
          this.model.NotifiedInvariant = inv.asInstanceOf[Invariant]; true
        })
        inv.notifySeqChanges(this, e._2, update, mOldValue, mNewValue)
        assert({
          this.model.NotifiedInvariant = null; true
        })
      }
      //perfom the changes on the mNewValue
    }
    updates = null
  }
}
