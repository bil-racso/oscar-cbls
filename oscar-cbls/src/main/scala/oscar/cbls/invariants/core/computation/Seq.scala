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
case class SeqInsert(AtPosition:SeqPosition,value:Int) extends SeqOperation
case class SeqRemove(position:SeqPosition) extends SeqOperation
case class SeqMoveAfter(start:SeqPosition,end:SeqPosition,moveAfter:SeqPosition, flip:Boolean) extends SeqOperation


trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:QList[SeqOperation], oldValue: IntSequence, newValue: IntSequence)
}

abstract class ChangingSeqValue(initialValue:Iterable[Int], initialDomain:Domain)
  extends AbstractVariable with SeqValue{

  //TODO: the Sequences should be coordinated somehow, just like PFDLL
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
  def insertAfter(pos:SeqPosition,value:Int):SeqPosition = {
    val toReturn = mNewValue.insertAfter(pos,value)
    updates = QList(SeqInsert(pos,value),updates)
    toReturn
  }

  def delete(pos:SeqPosition){
    mNewValue.delete(pos)
    updates = QList(SeqRemove(pos),updates)
  }

  def moveAfter(start:SeqPosition, end:SeqPosition, moveAfterTarget:SeqPosition, flip:Boolean){
    mNewValue.moveAfter(start,end,moveAfterTarget,flip)
    updates = QList(SeqMoveAfter(start,end,moveAfterTarget,flip),updates)
  }

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

/*
object ChangingSetValue{
  implicit val ord:Ordering[ChangingSetValue] = new Ordering[ChangingSetValue]{
    def compare(o1: ChangingSetValue, o2: ChangingSetValue) = o1.compare(o2)
  }
}

/**An IntSetVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is set of integer.
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSSetConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  * */
class CBLSSetVar(givenModel: Store, initialValue: SortedSet[Int], initialDomain:Domain, n: String = null)

case class CBLSSeqConst(override val value:Iterable[Int])
  extends SeqValue {
}
  }
*/