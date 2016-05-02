package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence

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
  def apply(s:UniqueIntSequence):UniqueIntSequence
}

//after is -1 for start position
case class SeqInsert(value:Int,pos:Int) extends SeqUpdate {
  override def apply(s : UniqueIntSequence) : UniqueIntSequence = s.insertAtPosition(value,pos)
}
case class SeqMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean) extends SeqUpdate{
  override def apply(s : UniqueIntSequence) : UniqueIntSequence = s.moveAfter(fromIncluded,toIncluded,after,flip)
}
case class SeqRemoveValue(value:Int) extends SeqUpdate {
  override def apply(s : UniqueIntSequence) : UniqueIntSequence = s.delete(s.positionOfValue(value).head)
}
case class SeqRemovePos(pos:Int) extends SeqUpdate {
  override def apply(s : UniqueIntSequence) : UniqueIntSequence = s.delete(pos)
}
case class SetORRestore(val value:UniqueIntSequence) extends SeqUpdate {
  //TODO: add some unique ID to the sequence value
  override def apply(s : UniqueIntSequence) : UniqueIntSequence = value
}

case class UpdateAndSeqAfter(seqBefore:Option[UpdateAndSeqAfter],update:SeqUpdate){
  private var mySeqAfter:UniqueIntSequence = null

  def valueAfterThisUpdate:UniqueIntSequence = {
    update match{
      case s:SetORRestore => s.value
      case _ =>
        if(mySeqAfter == null) mySeqAfter = update(seqBefore.head.valueAfterThisUpdate)
        mySeqAfter
    }
  }

  def valueBeforeAllUpdates:Option[UniqueIntSequence] =
    update match{
      case s:SetORRestore => None
      case _ => seqBefore.head.valueBeforeAllUpdates
  }

  def valueBeforeThisUpdate:Option[UniqueIntSequence] = seqBefore match{
    case None => None
    case Some(s) => Some(s.valueAfterThisUpdate)
  }

  def positionOfValueNew(value:Int):Option[Int] = {
    update match{
      case SetORRestore(s) => s.positionOfValue(value)
      case SeqInsert(value2,pos) => if(value == value2) Some(pos) else seqBefore.head.positionOfValueNew(value) match{
        case None => None
        case Some(p) => if (p < pos) Some(p) else Some(p+1)
      }
      case SeqRemoveValue(value2) => if(value == value2) None else seqBefore.head.positionOfValueNew(value) match{
        case None => None
        case Some(p) => if (p < seqBefore.head.positionOfValueNew(value2).head) Some(p) else Some(p-1)
      }
      case SeqRemovePos(pos) => throw new Error("TODO");null
      case _:SeqMove => throw new Error("TODO");null
    }
  }
  def valueAtPositionNew(pos:Int):Option[Int] = throw new Error("TODO")
  def containsNew(value:Int):Boolean = {
    update match{
      case SetORRestore(s) => s.contains(value)
      case SeqInsert(value2,pos) => value == value2 || seqBefore.head.containsNew(value)
      case SeqRemoveValue(value2) => value != value2 && seqBefore.head.containsNew(value)
      case SeqRemovePos(pos) => seqBefore.head.valueAtPositionNew(pos) match{
        case None => false
        case Some(value2) => value != value2 && seqBefore.head.containsNew(value)}
      case _:SeqMove => seqBefore.head.containsNew(value)
    }
  }

  def sizeNew:Int =
    update match{
      case SetORRestore(v) => v.size
      case _:SeqInsert => seqBefore.size +1
      case _:SeqRemoveValue => seqBefore.size -1
      case _:SeqRemovePos => seqBefore.size -1
      case _:SeqMove => seqBefore.size
    }
}

trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:UpdateAndSeqAfter)
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

  val mOldValue:UniqueIntSequence = UniqueIntSequence(initialValue,maxPivot,maxValue)
  var updates:UpdateAndSeqAfter = null

  override def value: UniqueIntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mOldValue
    model.propagate(this)
    mOldValue
  }

  def newValue:UniqueIntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    updates.valueAfterThisUpdate
  }

  /**these can be expressed on the newValue only (oldValue should trigger an exception*/

  //-1 for first position
  def insertAtPosition(value:Int,pos:Int){
    recordUpdate(SeqInsert(value,pos))
  }
  def deleteValue(value:Int){
    recordUpdate(SeqRemoveValue(value))
  }
  def deleteAtPosition(pos:Int){
    recordUpdate(SeqRemovePos(pos))
  }
  //-1 for first position
  def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    recordUpdate(SeqMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip))
  }
  def setORRestore(seq:UniqueIntSequence){
    updates = new UpdateAndSeqAfter(None,SetORRestore(value))
  }

  private def recordUpdate(update: SeqUpdate){
    updates =  new UpdateAndSeqAfter(Some(updates),update)
  }

  final protected def performSeqPropagation(): Unit = {
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
      inv.notifySeqChanges(this, e._2, updates)
      assert({
        this.model.NotifiedInvariant = null; true
      })
    }
    //perfom the changes on the mNewValue
    updates = null
  }
}


