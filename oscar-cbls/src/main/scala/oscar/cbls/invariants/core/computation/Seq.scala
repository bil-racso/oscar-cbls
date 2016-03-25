package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.{IntSequence, SeqPosition}

import scala.collection.immutable.SortedSet

sealed trait SeqValue extends Value{
  def value:IntSequence
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = "{" + value.mkString(",") + "}"
}

object SeqValue{
//  implicit def tist2IntSeqVar(a:List[Int]):SeqValue = CBLSSeqConst(a)
}

sealed abstract class SeqOperation
class SeqInsert(value:Int,after:Int) extends SeqOperation//after is -1 for start position
class SeqMove(from:Int,to:Int,after:Int,flip:Boolean) extends SeqOperation
class SeqRemove(value:Int) extends SeqOperation
class SeqSet(value:QList[Int]) extends SeqOperation

trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:QList[SeqOperation], oldValue: IntSequence, newValue: IntSequence)
}

abstract class ChangingSeqValue(initialValue:Iterable[Int], initialDomain:Domain)
  extends AbstractVariable with SeqValue{

  val mNewValue:IntSequence = new IntSequence()
  val mOldValue:IntSequence = new IntSequence()

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

  var updates:QList[SeqOperation] = null

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/
  def insertAfter(pos:Int,value:Int)
  def delete(value:Int)
  def move(from:Int,to:Int,after:Int,flip:Boolean)
  def set(seq:QList[Int])

  final protected def performSeqPropagation(): Unit = {
    if(updates!=null){
      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        currentElement = currentElement.next
        val inv:SeqNotificationTarget = e._1.asInstanceOf[SeqNotificationTarget]
        assert({this.model.NotifiedInvariant=inv.asInstanceOf[Invariant]; true})
        inv.notifySeqChanges(this,e._2,updates,mOldValue,mNewValue)
        assert({this.model.NotifiedInvariant=null; true})
      }
      //perfom the changes on the mNewValue

    }
    updates = null
  }
}
