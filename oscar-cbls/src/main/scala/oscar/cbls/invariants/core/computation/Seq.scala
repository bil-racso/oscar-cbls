package oscar.cbls.invariants.core.computation

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

import oscar.cbls.invariants.core.algo.fun.PiecewiseLinearBijectionNaive
import oscar.cbls.invariants.core.algo.seq.functional._
import oscar.cbls.invariants.core.propagation.Checker

import scala.language.implicitConversions

sealed trait SeqValue extends Value{
  def value:IntSequence
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = value.toString
}

object SeqValue{
  implicit def tist2IntSeqVar(a:List[Int]):SeqValue = CBLSSeqConst(IntSequence(a))
}


//TODO: when instantiating moves, we must always check that they cannot be anihilated.
//basically, move instantiation should proceed through obects that perfor such anihilation automatically, and based on move features, not on quichEquals.

sealed abstract class SeqUpdate(val newValue:IntSequence){
  protected[computation] def reverse(target:IntSequence, from:SeqUpdate = SeqUpdateLastNotified(newValue)):SeqUpdate
  protected[computation] def regularize(maxPivot:Int):SeqUpdate
  protected[computation] def prepend(u:SeqUpdate):SeqUpdate
  def depth:Int
}

sealed abstract class SeqUpdateWithPrev(val prev:SeqUpdate,newValue:IntSequence) extends SeqUpdate(newValue) {
  def oldPosToNewPos(oldPos:Int):Option[Int]
  def newPos2OldPos(newPos:Int):Option[Int]
  override val depth:Int = {
    val pd = prev.depth
    if(pd >=0) pd+1 else pd-1
  }
}

object SeqUpdateInsert {
  def apply(value : Int, pos : Int, prev : SeqUpdate, seq : IntSequence) : SeqUpdate = {
    prev match {
      //we compare the seq here because seq equality is used for checkpointing stuff to anihilate the moves
      case x@SeqUpdateRemove(removedPosition : Int, prevOfDelete : SeqUpdate)
        if prev.newValue quickEquals seq => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,prev.newValue.insertAtPosition(value, pos, fast = true))
    }
  }

  def apply(value : Int, pos : Int, prev : SeqUpdate) : SeqUpdate = {
    prev match {
      //here, since there is no seq given, we compare on the move itself to anihilate the moves
      case x@SeqUpdateRemove(removedPosition : Int, prevOfDelete : SeqUpdate) if pos == removedPosition && value == x.removedValue => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,prev.newValue.insertAtPosition(value, pos, fast = true))
    }
  }

  /**
   *
   * @param i
   * @return value, position, prev
   */
  def unapply(i:SeqUpdateInsert):Option[(Int,Int,SeqUpdate)] = Some(i.value,i.pos,i.prev)
}


//after is -1 for start position
class SeqUpdateInsert(val value:Int,val pos:Int,prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){
  assert(seq equals prev.newValue.insertAtPosition(value,pos,fast=true))

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverse(target,SeqUpdateRemove(pos,newPrev,prev.newValue))
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos < pos) Some(oldPos)
    else Some(oldPos + 1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos == pos) None
    else if (newPos < pos) Some(newPos)
    else Some(newPos-1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateInsert(value,pos,prev,seq.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateInsert(value,pos,prev.prepend(u),seq)

  override def toString : String = "SeqUpdateInsert(value:" + value + " pos:" + pos + " prev:" + prev + ")"
}

object SeqUpdateMove{
  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate):SeqUpdateMove =
    new SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))

  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate,seq:IntSequence):SeqUpdate = {
    prev match{
      case u:SeqUpdateMove if u.prev.newValue quickEquals seq => u.prev
      case _ => new SeqUpdateMove(fromIncluded,toIncluded,after, flip, prev, seq)
    }
  }

  /**
   *
   * @param move
   * @return fromIncluded,toIncluded,after,flip,prev
   */
  def unapply(move:SeqUpdateMove):Option[(Int,Int,Int,Boolean,SeqUpdate)] = Some(move.fromIncluded,move.toIncluded,move.after,move.flip,move.prev)
}


class SeqUpdateMove(val fromIncluded:Int,val toIncluded:Int,val after:Int, val flip:Boolean, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){
  assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true), "given seq=" + seq + " should be " +  prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head

  def movedValues = prev.newValue.valuesBetweenPositions(fromIncluded,toIncluded)

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    val (intFromIncluded,intToIncluded) = if(flip) (toIncluded,fromIncluded) else (fromIncluded,toIncluded)
    prev.reverse(target,new SeqUpdateMove(oldPosToNewPos(intFromIncluded).head, oldPosToNewPos(intToIncluded).head, oldPosToNewPos(fromIncluded-1).head, flip, newPrev,prev.newValue))
  }

  assert({seq match{case m:MovedIntSequence => m.localBijection.checkBijection() case _ => ;};true})

  //TODO: find O(1) solution
  private var localBijection:PiecewiseLinearBijectionNaive = null
  private def ensureBijection(){
    if(localBijection == null) localBijection = MovedIntSequence.bijectionForMove(fromIncluded, toIncluded, after, flip)
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    ensureBijection()
    Some(localBijection.backward(oldPos))
  }
  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    ensureBijection()
    Some(localBijection.forward(newPos))
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,seq.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev.prepend(u),seq)

  override def toString : String = "SeqUpdateMove(fromIncluded:" + fromIncluded + " toIncluded:" + toIncluded+ " after:" + after+ " flip:" + flip + " prev:" + prev + ")"
}

object SeqUpdateRemove {

  def apply(position : Int, prev : SeqUpdate):SeqUpdate = {
    apply(position,prev,prev.newValue.delete(position, fast = true))
  }

  def apply(position : Int, prev : SeqUpdate, seq:IntSequence):SeqUpdate = {
    prev match {
      case SeqUpdateInsert(insertedValue:Int,insertPos:Int,insertPrev:SeqUpdate)
        if insertPos == position && seq.valueAtPosition(position).head == insertedValue
        //NB: must check first for pos to avoid error on .head
      => insertPrev
      case _ => new SeqUpdateRemove(position,prev,seq)
    }
  }

  /**
   *
   * @param r
   * @return position,prev
   */
  def unapply(r:SeqUpdateRemove):Option[(Int,SeqUpdate)] = Some(r.position,r.prev)
}

class SeqUpdateRemove(val position:Int,prev:SeqUpdate,seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){

  assert(seq equals prev.newValue.delete(position,fast=true))

  val removedValue:Int = seq match{case d:RemovedIntSequence => d.removedValue case _ => prev.newValue.valueAtPosition(position).head}

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverse(target,SeqUpdateInsert(removedValue, position, newPrev, prev.newValue))
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos == position) None
    else if (oldPos < position) Some(oldPos)
    else Some(oldPos-1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos <= position) Some(newPos)
    else Some(newPos +1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateRemove(position,prev,seq.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateRemove(position,prev.prepend(u),seq)

  override def toString : String =  "SeqUpdateRemove(value:" + removedValue + " position:" + position + " prev:" + prev + ")"
}

case class SeqUpdateSet(value:IntSequence) extends SeqUpdate(value){
  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateSet (target)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateSet(value.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = this

  override def depth : Int = -1
}

case class SeqUpdateLastNotified(value:IntSequence) extends SeqUpdate(value){
  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateSet (target)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateLastNotified(value.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    require(u.newValue quickEquals value)
    u
  }

  override def depth : Int = 0
}

object SeqUpdateDefineCheckpoint{

  def apply(prev:SeqUpdate,activeCheckpoint:Boolean, maxPivotPerValuePercent:Int):SeqUpdateDefineCheckpoint = {
    new SeqUpdateDefineCheckpoint(prev,activeCheckpoint, maxPivotPerValuePercent)
  }

  def unapply(u:SeqUpdateDefineCheckpoint):Option[(SeqUpdate,Boolean)] = Some(u.prev,u.activeCheckpoint)
}

class SeqUpdateDefineCheckpoint(prev:SeqUpdate,val activeCheckpoint:Boolean, maxPivotPerValuePercent:Int)
  extends SeqUpdateWithPrev(prev,prev.newValue.regularizeToMaxPivot(maxPivotPerValuePercent)){
  protected[computation]  def reverse(target : IntSequence, from : SeqUpdate) : SeqUpdate = prev.reverse(target,from)

  protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  def oldPosToNewPos(oldPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  def newPos2OldPos(newPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = SeqUpdateDefineCheckpoint(prev.prepend(u),activeCheckpoint,maxPivotPerValuePercent)

  override def toString : String = "SeqUpdateDefineCheckpoint(prev:" + prev + ")"
}

object SeqUpdateRollBackToCheckpoint{
  def apply(checkpointValue:IntSequence,instructionsThatMustBeUndone:SeqUpdate):SeqUpdateRollBackToCheckpoint = {
    new SeqUpdateRollBackToCheckpoint(checkpointValue, instructionsThatMustBeUndone)
  }

  def unapply(u:SeqUpdateRollBackToCheckpoint):Option[IntSequence] = Some(u.checkpointValue)
}

class SeqUpdateRollBackToCheckpoint(val checkpointValue:IntSequence,instructionsThatMustBeUndone:SeqUpdate) extends SeqUpdate(checkpointValue){
  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateSet (target)
  }

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = this

  private var reversedInstructions:SeqUpdate = null
  def howToRollBack:SeqUpdate = {
    if (reversedInstructions != null) reversedInstructions
    else {
      reversedInstructions = instructionsThatMustBeUndone.reverse(checkpointValue)
      reversedInstructions
    }
  }

  override def toString : String =
    "SeqUpdateRollBackToCheckpoint(checkpoint:" + checkpointValue + ")" //+ " howTo:" +  howToRollBack + ")"

  override def depth : Int = 0
}

/**
 * this is the thing you must implement to listen to any ChangingSeqValue.
 * you will be notified about seqChanges through this interface
 * notice that you will always be notified of checkpoint-related changes.
 * Invariants must only consider one hcackpoint, since they are never notified about checkpoint release,
 * only about newly defined checkpoints.
 * if you decide not to handle checkpoint, you will anyway be notified about rollbacks, but the rollback actualy
 * includes incremental changes info, so you can go for incremental changes in this way.
 *
 * notice that checkpoint definition is sent as any other update (although it is identity operator)
 */
trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate)
}

class CBLSSeqConst(override val value:ConcreteIntSequence) extends SeqValue{
  override def domain : Domain = value.largestValue match{case None => Domain.empty case Some(v) => 0 to v}
  override def name : String = value.toString
}

object CBLSSeqConst{
  implicit def seq2SeqValue(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())
  implicit def seq2SeqConst(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())

  def apply(seq:IntSequence):CBLSSeqConst = new CBLSSeqConst(seq.regularize())
}

class CBLSSeqVar(givenModel:Store, initialValue:IntSequence, val maxVal:Int = Int.MaxValue, n: String = null, maxPivotPerValuePercent:Int = 4, maxHistorySize:Int = 50)
  extends ChangingSeqValue(initialValue, maxVal, maxPivotPerValuePercent, maxHistorySize) with Variable{
  require(domain.min == 0)
  require(givenModel != null)

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  //-1 for first position
  override def insertAtPosition(value:Int,pos:Int){
    super.insertAtPosition(value,pos)
  }

  override  def remove(position:Int){
    super.remove(position)
  }

  //-1 for first position
  override  def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    super.move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean)
  }

  override def flip(fromIncludedPosition:Int,toIncludedPosition:Int){
    super.flip(fromIncludedPosition,toIncludedPosition)
  }
  override  def setValue(seq:IntSequence) {super.setValue(seq)}

  override  def :=(seq:IntSequence) {super.setValue(seq)}

  override def defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean):IntSequence = {
    super.defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean)
  }

  override def rollbackToCurrentCheckpoint(checkpoint:IntSequence) {
    super.rollbackToCurrentCheckpoint(checkpoint)
  }

  override def releaseCurrentCheckpointAtCheckpoint(){
    super.releaseCurrentCheckpointAtCheckpoint()
  }

  def <==(i: SeqValue) {IdentitySeq(i,this)}

  def createClone(maxDepth:Int=50):CBLSSeqVar = {
    val clone = new CBLSSeqVar(model,this.value,this.maxValue,"clone_of_" + this.name,maxPivotPerValuePercent,maxDepth)
    IdentitySeq(this,clone)
    clone
  }

  override def performPropagation(){performSeqPropagation()}
}

object CBLSSeqVar{
  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }
}

class ChangingSeqValueSnapShot(val variable:ChangingSeqValue,val savedValue:IntSequence) extends AbstractVariableSnapShot(variable){
  override protected def doRestore() : Unit = {variable.asInstanceOf[CBLSSeqVar] := savedValue}
}

abstract class ChangingSeqValue(initialValue: Iterable[Int], val maxValue: Int, val maxPivotPerValuePercent: Int, val maxHistorySize:Int)
  extends AbstractVariable with SeqValue{

  override def snapshot : ChangingSeqValueSnapShot = new ChangingSeqValueSnapShot(this,this.value)

  def valueAtSnapShot(s:Snapshot):IntSequence = s(this) match{case s:ChangingSeqValueSnapShot => s.savedValue case _ => throw new Error("cannot find value of " + this + " in snapshot")}

  //This section of code is for maintining the checkpoint stack.
  //stack does not include top checkpoint
  private var checkpointStackNotTop:List[(IntSequence,SeqUpdate,Boolean)] = List.empty
  private var topCheckpointIsActive:Boolean = false
  private var topCheckpointIsActiveDeactivated:Boolean = false
  private var topCheckpoint:IntSequence = null //can be null if no checkpoint
  private var notifiedSinceTopCheckpoint:SeqUpdate = null //what has been done after the current checkpoint (not maintained if checkpoint is not active)

  private def recordNotifiedChangesForCheckpoint(toNotify:SeqUpdate){
    //includes checkpoint declaration, so we have to browse through them when concatenating the sequences
    toNotify match{
      case SeqUpdateLastNotified(value:IntSequence) =>
        //nothing to do :-)
        if(notifiedSinceTopCheckpoint != null)
          require(notifiedSinceTopCheckpoint.newValue quickEquals value, "notifiedSinceTopCheckpoint.newValue=" + notifiedSinceTopCheckpoint.newValue  + " got " + value)
      case c@SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
        //we have to push the current checkpoint, and create a new one
        recordNotifiedChangesForCheckpoint(prev)
        val checkpoint = toNotify.newValue
        if(topCheckpoint != null){
          checkpointStackNotTop = (topCheckpoint,notifiedSinceTopCheckpoint,topCheckpointIsActive) :: checkpointStackNotTop
        }
        topCheckpointIsActive = isActive
        topCheckpointIsActiveDeactivated = false
        topCheckpoint = checkpoint
        notifiedSinceTopCheckpoint = SeqUpdateLastNotified(checkpoint)

      case SeqUpdateInsert(value,pos,prev:SeqUpdate) =>
        recordNotifiedChangesForCheckpoint(prev)
        if(notifiedSinceTopCheckpoint != null) notifiedSinceTopCheckpoint = SeqUpdateInsert(value,pos,notifiedSinceTopCheckpoint,toNotify.newValue)
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        recordNotifiedChangesForCheckpoint(prev)
        if(notifiedSinceTopCheckpoint != null) notifiedSinceTopCheckpoint = SeqUpdateMove(fromIncluded,toIncluded,after,flip,notifiedSinceTopCheckpoint,toNotify.newValue)
      case SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
        recordNotifiedChangesForCheckpoint(prev)
        if(notifiedSinceTopCheckpoint != null) notifiedSinceTopCheckpoint = SeqUpdateRemove(position,notifiedSinceTopCheckpoint,toNotify.newValue)
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        if(notifiedSinceTopCheckpoint != null) notifiedSinceTopCheckpoint = SeqUpdateLastNotified(checkpoint)
      case SeqUpdateSet(value:IntSequence) =>
        if(notifiedSinceTopCheckpoint != null) notifiedSinceTopCheckpoint = SeqUpdateSet(value)
      case _ => throw new Error("unexpected change?")
    }
  }

  private def popTopCheckpoint(checkpoint:IntSequence){
    assert(toNotify.newValue equals topCheckpoint)
    require(!topCheckpointIsActive || topCheckpointIsActiveDeactivated || (toNotify.newValue quickEquals topCheckpoint))

    checkpointStackNotTop match{
      case top :: tail =>
        checkpointStackNotTop = tail
        topCheckpoint = top._1
        topCheckpointIsActive = top._3
        notifiedSinceTopCheckpoint = if(topCheckpointIsActive) notifiedSinceTopCheckpoint.prepend(top._2) else null
        topCheckpointIsActiveDeactivated = topCheckpointIsActive
      case Nil =>
        //there is no upper checkpoint
        topCheckpoint = null
        notifiedSinceTopCheckpoint = null
        topCheckpointIsActive = false
        topCheckpointIsActiveDeactivated = false
    }
  }

  private def instructionsToRollBackToTopCheckpoint(checkpoint:IntSequence, startingFrom:IntSequence):SeqUpdate = {
    require(topCheckpoint!=null)
    require(startingFrom quickEquals notifiedSinceTopCheckpoint.newValue)
    notifiedSinceTopCheckpoint.reverse(checkpoint)
  }

  //end of the checkpoint stack stuff

  private var mOldValue:IntSequence = IntSequence(initialValue)
  protected[computation] var toNotify:SeqUpdate = SeqUpdateLastNotified(mOldValue)

  override def domain : Domain = 0 to maxValue
  override def max : Int = maxValue
  override def min : Int = 0

  override def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    model.propagate(this)
    mOldValue
  }

  def newValue:IntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    toNotify.newValue
  }

  override def toString:String = name + ":=" + (if(model.propagateOnToString) value else toNotify.newValue)

  def toStringNoPropagate: String = name + ":=" + toNotify.newValue.toString()

  def trimToNotifyIfNeeded(){
    if(math.abs(toNotify.depth) > maxHistorySize){
      toNotify = trimToLatestCheckpoint(toNotify)
    }
  }

  def trimToLatestCheckpoint(updates:SeqUpdate):SeqUpdate = {
    updates match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        SeqUpdateInsert(value, pos, trimToLatestCheckpoint(prev), updates.newValue)
      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        SeqUpdateMove(fromIncluded, toIncluded, after, flip, trimToLatestCheckpoint(prev), updates.newValue)
      case SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        SeqUpdateRemove(position, trimToLatestCheckpoint(prev), updates.newValue)
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActiveCheckpoint : Boolean) =>
        SeqUpdateDefineCheckpoint(SeqUpdateSet(updates.newValue), isActiveCheckpoint, maxPivotPerValuePercent)

      case SeqUpdateRollBackToCheckpoint(checkpointValue : IntSequence) => updates
      case SeqUpdateSet(value : IntSequence) => updates
      case SeqUpdateLastNotified(value : IntSequence) => updates
    }
  }


    //TODO: -1 for first position
    protected def insertAtPosition(value:Int,pos:Int){
      assert(pos <= toNotify.newValue.size)
      assert(pos >= 0)
      toNotify = SeqUpdateInsert(value,pos,toNotify)
      trimToNotifyIfNeeded()
      notifyChanged()
    }

    protected def remove(position:Int){
      require(toNotify.newValue.size > position && position >=0, "removing at position " + position + " size is " + newValue.size)
      toNotify = SeqUpdateRemove(position,toNotify)
      trimToNotifyIfNeeded()
      notifyChanged()
    }

    protected def flip(fromIncludedPosition:Int,toIncludedPosition:Int){
      move(fromIncludedPosition,toIncludedPosition,fromIncludedPosition-1,true)
    }

    //-1 for first position
    protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
      require(toNotify.newValue.size > fromIncludedPosition)
      require(toNotify.newValue.size > toIncludedPosition)
      require(toNotify.newValue.size > afterPosition)
      require(0 <= fromIncludedPosition)
      require(0<=toIncludedPosition)
      require(-1<=afterPosition)
      require(fromIncludedPosition <= toIncludedPosition)
      toNotify  = SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,toNotify)
      trimToNotifyIfNeeded()
      notifyChanged()
    }

    protected [computation] def setValue(seq:IntSequence){
      //since we will override and lose the content of the toNotify, we have to ensure that there is no checkpoint delcared in this before
      pushCheckPoints(toNotify,false)
      toNotify = SeqUpdateSet(seq)
      trimToNotifyIfNeeded()
      notifyChanged()
    }

    //returns the update since the last defined checkpoint
    private def pushCheckPoints(updates:SeqUpdate,outputNeeded:Boolean):SeqUpdate = {
      updates match{
        case x@SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
          if(outputNeeded) SeqUpdateInsert(value,pos,pushCheckPoints(prev,true),x.newValue)
          else pushCheckPoints(prev,false)
        case x@SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
          if(outputNeeded) SeqUpdateMove(fromIncluded,toIncluded,after,flip,pushCheckPoints(prev,true),x.newValue)
          else pushCheckPoints(prev,false)
        case x@SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
          if(outputNeeded) SeqUpdateRemove(position,pushCheckPoints(prev,true),x.newValue)
          else pushCheckPoints(prev,false)
        case SeqUpdateSet(value:IntSequence) =>
          updates
        case SeqUpdateLastNotified(value:IntSequence) =>
          updates
        case x@SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActiveCheckpoint:Boolean) =>
          //We have to push a checkpoint here!
          val updatesSincePrevCheckpoint = pushCheckPoints(prev,true)

          recordNotifiedChangesForCheckpoint(updatesSincePrevCheckpoint)
          recordNotifiedChangesForCheckpoint(SeqUpdateDefineCheckpoint(SeqUpdateLastNotified(updatesSincePrevCheckpoint.newValue),isActiveCheckpoint,maxPivotPerValuePercent))

          require(updates.newValue quickEquals updatesSincePrevCheckpoint.newValue)
          SeqUpdateLastNotified(updates.newValue)
        case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence) =>
          //must be the top checkpoint of the stack!
          require(checkpointValue quickEquals topCheckpoint)
          SeqUpdateLastNotified(checkpointValue)
      }
    }

    protected def defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean):IntSequence = {
      toNotify = SeqUpdateDefineCheckpoint(toNotify.regularize(maxPivotPerValuePercent),checkPointIsActive,maxPivotPerValuePercent)
      trimToNotifyIfNeeded()
      notifyChanged()
      toNotify.newValue
    }

    protected def rollbackToCurrentCheckpoint(checkpoint:IntSequence) = {
      //check that the checkpoint is declared in the toNotify, actually.

      //println("notified of rollBack toNotify:" + toNotify + " currentCheckpoint:" + topCheckpoint + " notifiedSinceTopCheckpoint:" + notifiedSinceTopCheckpoint)

      val attemptByCleaningToNotify = popToNotifyUntilCheckpointDeclaration(toNotify,checkpoint)
      if(attemptByCleaningToNotify == null){
        //it means that the checkpont has been communicated :-)
        if(topCheckpointIsActive) {
          //we must do it incrementally, or through rollBack update
          if (topCheckpointIsActiveDeactivated) {
            //it has been deactivated, so it must be performed by inverting the instructions
            toNotify = instructionsToRollBackToTopCheckpoint(checkpoint, this.mOldValue)
          } else {
            toNotify = SeqUpdateRollBackToCheckpoint(checkpoint,notifiedSinceTopCheckpoint)
          }
        }else{
          //Asking for rollback on an inactive checkpoint, we go for a set...
          toNotify = SeqUpdateSet(checkpoint)
        }
        notifyChanged()
      }else{
        //we could roll back to a set to the checkpoint, the checkpoint define,
        // or whatever actually gets to the checkpoint, so nothing to do, actually.
        toNotify = attemptByCleaningToNotify
        require(toNotify.newValue quickEquals checkpoint)
        toNotify match{
          case u:SeqUpdateLastNotified => ;
          case _ => notifyChanged()
        }
      }
      //println("after notified of rollBack toNotify:" + toNotify + " currentCheckpoint:" + topCheckpoint + " notifiedSinceTopCheckpoint:" + notifiedSinceTopCheckpoint)

    }

    /**
     * @param updates
     * @param checkpoint
     * @return the cleaned sequence, null if cleaning the sequence could not rollback to the checkpoint
     */
    private def popToNotifyUntilCheckpointDeclaration(updates:SeqUpdate,checkpoint:IntSequence):SeqUpdate = {
      updates match{
        case x@SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
          popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
        case x@SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
          popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
        case x@SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
          popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
        case SeqUpdateSet(value:IntSequence) =>
          if(value quickEquals checkpoint){
            updates
          }else{
            null
          }
        case SeqUpdateLastNotified(value:IntSequence) =>
          //check for equality
          if(value quickEquals checkpoint){
            updates
          }else{
            null
          }
        case x@SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
          //here
          require(updates.newValue quickEquals checkpoint)
          updates
        case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence) =>
          require(checkpointValue quickEquals checkpoint)
          updates
      }
    }

    protected def releaseCurrentCheckpointAtCheckpoint(){
      //TODO: test this extensively
      val checkpoint = toNotify.newValue
      assert(toNotify.newValue equals topCheckpoint)
      require(!topCheckpointIsActive || topCheckpointIsActiveDeactivated || (toNotify.newValue quickEquals topCheckpoint))
      notifiedSinceTopCheckpoint = SeqUpdateLastNotified(checkpoint)

      val attemptToCleanToNotify:SeqUpdate = popToNotifyUntilCheckpointDeclaration(toNotify,checkpoint)
      attemptToCleanToNotify match{
        case SeqUpdateDefineCheckpoint(prev:SeqUpdate,active) =>
          //we have found the checkpoint declaration, we can just remove it as it has never been communicated, actually
          toNotify = prev
        case null =>
          //no declaration was found, we need to impact the checkpoint stack
          popTopCheckpoint(checkpoint)
        case SeqUpdateSet(value) =>
          //this is a set to the checkpoint
          require(value quickEquals checkpoint)
          //no declaration was found, we need to impact the checkpoint stack
          toNotify = attemptToCleanToNotify
          popTopCheckpoint(checkpoint)
        case SeqUpdateLastNotified(value) =>
          //we just notified about getting to this checkpoint
          //we need to impact the checkpoint stack
          require(value quickEquals checkpoint)
          toNotify = attemptToCleanToNotify
          popTopCheckpoint(checkpoint)
        case SeqUpdateRollBackToCheckpoint(value) =>
          require(value quickEquals checkpoint)
          //no declaration was found, we need to impact the checkpoint stack
          toNotify = attemptToCleanToNotify
          popTopCheckpoint(checkpoint)
      }
    }

    final protected def performSeqPropagation() = {
      privatePerformSeqPropagation(false)
    }

    //returns the new value
    protected[computation] def privatePerformSeqPropagation(stableCheckpoint:Boolean): IntSequence = {
      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next

      if(stableCheckpoint || !topCheckpointIsActive) toNotify = toNotify.regularize(maxPivotPerValuePercent)

      while (currentElement != headPhantom) {
        val e = currentElement.elem
        currentElement = currentElement.next
        val inv : SeqNotificationTarget = e._1.asInstanceOf[SeqNotificationTarget]
        assert({
          this.model.NotifiedInvariant = inv.asInstanceOf[Invariant]; true
        })
        inv.notifySeqChanges(this, e._2, toNotify)
        assert({
          this.model.NotifiedInvariant = null; true
        })
      }

      recordNotifiedChangesForCheckpoint(toNotify)

      val theNewValue = toNotify.newValue
      val start = SeqUpdateLastNotified(theNewValue)
      mOldValue = theNewValue
      toNotify = start

      theNewValue
    }

    protected def :=(seq:IntSequence){
      setValue(seq)
      notifyChanged()
    }
  }

  /** this is a special case of invariant that has a single output variable, that is a Seq
    * @author renaud.delandtsheer@cetic.be
    */
  abstract class SeqInvariant(initialValue:IntSequence, maxValue:Int = Int.MaxValue, maxPivotPerValuePercent:Int = 10, maxHistorySize:Int = 10)
    extends ChangingSeqValue(initialValue, maxValue:Int, maxPivotPerValuePercent, maxHistorySize)
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
    def apply(fromValue:SeqValue,toValue:CBLSSeqVar){
      fromValue match{
        case c:CBLSSeqConst => toValue := c.value
        case c:ChangingSeqValue => new IdentitySeq(c,toValue)
      }
    }
  }

  class IdentitySeq(fromValue:ChangingSeqValue, toValue:CBLSSeqVar)
    extends Invariant
    with SeqNotificationTarget{

    registerStaticAndDynamicDependency(fromValue)
    toValue.setDefiningInvariant(this)
    finishInitialization()

    toValue := fromValue.value

    override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
      assert(v == fromValue)
      digestChanges(changes)
      //println("IdentitySeq notified " + changes)
    }

    private var currentCheckpoint:IntSequence = null

    def digestChanges(changes:SeqUpdate){
      changes match{
        case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
          digestChanges(prev)
          toValue.insertAtPosition(value,pos)
        case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
          digestChanges(prev)
          toValue.move(fromIncluded,toIncluded,after,flip)
        case SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
          digestChanges(prev)
          toValue.remove(position)
        case SeqUpdateSet(s) =>
          toValue.setValue(s)
        case SeqUpdateLastNotified(value:IntSequence) =>
          //nothing to do here
          assert(value equals toValue.newValue)
        case SeqUpdateRollBackToCheckpoint(value:IntSequence) =>
          toValue.rollbackToCurrentCheckpoint(currentCheckpoint)
        case SeqUpdateDefineCheckpoint(prev:SeqUpdate,activeCheckpoint:Boolean) =>
          digestChanges(prev)
          currentCheckpoint = toValue.defineCurrentValueAsCheckpoint(activeCheckpoint)
      }
    }

    override def checkInternals(c:Checker){
      c.check(toValue.value equals fromValue.value, Some("IdentitySeq: toValue.value=" +toValue.value + " should equal fromValue.value=" + fromValue.value))
    }
  }
