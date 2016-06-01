package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.algo.seq.functional._
import oscar.cbls.invariants.core.propagation.Checker

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


//TODO: when instantiating moves, we must always check that they cannot be anihilated.
//basically, move instantiation should proceed through obects that perfor such anihilation automatically, and based on move features, not on quichEquals.

sealed abstract class SeqUpdate(val newValue:UniqueIntSequence){
  protected[computation] def reverse(target:UniqueIntSequence, from:SeqUpdate = SeqUpdateLastNotified(newValue)):SeqUpdate
  protected[computation] def regularize:SeqUpdate
  protected[computation] def prepend(u:SeqUpdate):SeqUpdate
}

sealed abstract class SeqUpdateWithPrev(val prev:SeqUpdate,newValue:UniqueIntSequence) extends SeqUpdate(newValue) {
  def oldPosToNewPos(oldPos:Int):Option[Int]
  def newPos2OldPos(newPos:Int):Option[Int]
}

object SeqUpdateInsert {
  def apply(value : Int, pos : Int, prev : SeqUpdate, seq : UniqueIntSequence) : SeqUpdate = {
    prev match {
      //we compare the seq here because seq equality is used for checkpointing stuff to anihilate the moves
      case x@SeqUpdateRemoveValue(removedValue : Int, prevOfDelete : SeqUpdate) if (prev.newValue quickEquals seq) => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,prev.newValue.insertAtPosition(value, pos, fast = true))
    }
  }

  def apply(value : Int, pos : Int, prev : SeqUpdate) : SeqUpdate = {
    prev match {
      //here, since there is no seq given, we compare on the move itself to anihilate the moves
      case x@SeqUpdateRemoveValue(removedValue : Int, prevOfDelete : SeqUpdate) if value == removedValue => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,prev.newValue.insertAtPosition(value, pos, fast = true))
    }
  }

  def unapply(i:SeqUpdateInsert):Option[(Int,Int,SeqUpdate)] = Some(i.value,i.pos,i.prev)
}


//after is -1 for start position
class SeqUpdateInsert(val value:Int,val pos:Int,prev:SeqUpdate, seq:UniqueIntSequence)
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){
  assert(seq equals prev.newValue.insertAtPosition(value,pos,fast=true))
  override protected[computation] def reverse(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverse(target,SeqUpdateRemoveValue(value,newPrev,prev.newValue))
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

  override protected[computation] def regularize : SeqUpdate =
    SeqUpdateInsert(value,pos,prev,seq.regularize())

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateInsert(value,pos,prev.prepend(u),seq)

  override def toString : String = "SeqUpdateInsert(value:" + value + " pos:" + pos + " prev:" + prev + ")"
}

object SeqUpdateMove{
  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate):SeqUpdateMove =
    new SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))

  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate,seq:UniqueIntSequence):SeqUpdate = {
    prev match{
      case u:SeqUpdateMove if u.prev.newValue quickEquals seq => u.prev
      case _ => new SeqUpdateMove(fromIncluded,toIncluded,after, flip, prev, seq)
    }
  }
  def unapply(move:SeqUpdateMove):Option[(Int,Int,Int,Boolean,SeqUpdate)] = Some(move.fromIncluded,move.toIncluded,move.after,move.flip,move.prev)
}


class SeqUpdateMove(val fromIncluded:Int,val toIncluded:Int,val after:Int, val flip:Boolean, prev:SeqUpdate, seq:UniqueIntSequence)
  extends SeqUpdateWithPrev(prev,seq){
  assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))
  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head

  override protected[computation] def reverse(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    //TODO: chek this!!!
    prev.reverse(target,SeqUpdateMove(after - (toIncluded - fromIncluded), after, fromIncluded-1, flip, newPrev,prev.newValue))
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = Some(seq.asInstanceOf[MovedUniqueIntSequence].localBijection.backward(oldPos))

  override def newPos2OldPos(newPos : Int) : Option[Int] = Some(seq.asInstanceOf[MovedUniqueIntSequence].localBijection.forward(newPos))

  override protected[computation] def regularize : SeqUpdate = SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,seq.regularize())

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev.prepend(u),seq)

  override def toString : String = "SeqUpdateMove(fromIncluded:" + fromIncluded + " toIncluded:" + toIncluded+ " after:" + after+ " flip:" + flip + " prev:" + prev + ")"
}

object SeqUpdateRemoveValue {

  def apply(value : Int, prev : SeqUpdate):SeqUpdate = {
    apply(value,prev,prev.newValue.delete(prev.newValue.positionOfValue(value).head, fast = true))
  }

  def apply(value : Int, prev : SeqUpdate, seq:UniqueIntSequence):SeqUpdate = {
    prev match {
      case SeqUpdateInsert(insertedValue:Int,insertPos:Int,insertPrev:SeqUpdate) if value == insertedValue && insertPos == seq.positionOfValue(value).head => insertPrev
      case _ => SeqUpdateRemoveValue(value,prev,seq)
    }
  }

  def unapply(r:SeqUpdateRemoveValue):Option[(Int,SeqUpdate)] = Some(r.value,r.prev)
}

class SeqUpdateRemoveValue(val value:Int,prev:SeqUpdate,seq:UniqueIntSequence)
  extends SeqUpdateWithPrev(prev,seq){
  assert(seq equals prev.newValue.delete(prev.newValue.positionOfValue(value).head,fast=true))

  def position:Int = prev.newValue.positionOfValue(value).head

  override protected[computation] def reverse(target:UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    prev.reverse(target,SeqUpdateInsert(value, position, newPrev, prev.newValue))
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

  override protected[computation] def regularize : SeqUpdate = SeqUpdateRemoveValue(value,prev,seq.regularize())

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateRemoveValue(value,prev.prepend(u),seq)

  override def toString : String =  "SeqUpdateRemoveValue(value:" + value + " prev:" + prev + ")"
}

case class SeqUpdateSet(value:UniqueIntSequence) extends SeqUpdate(value){
  override protected[computation] def reverse(target : UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateSet (target)
  }

  override protected[computation] def regularize : SeqUpdate = SeqUpdateSet(value.regularize())

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = this
}

case class SeqUpdateLastNotified(value:UniqueIntSequence) extends SeqUpdate(value){
  override protected[computation] def reverse(target : UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateSet (target)
  }

  override protected[computation] def regularize : SeqUpdate = SeqUpdateSet(value.regularize())

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    require(u.newValue quickEquals value)
    u
  }
}


case class SeqUpdateDefineCheckpoint(mprev:SeqUpdate,activeCheckpoint:Boolean)
  extends SeqUpdateWithPrev(mprev,mprev.newValue.regularize()){
  protected[computation]  def reverse(target : UniqueIntSequence, from : SeqUpdate) : SeqUpdate = mprev.reverse(target,from)

  protected[computation] def regularize : SeqUpdate = this

  def oldPosToNewPos(oldPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  def newPos2OldPos(newPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = SeqUpdateDefineCheckpoint(mprev.prepend(u),activeCheckpoint)
}


object SeqUpdateRollBackToCheckpoint{
  def apply(checkpointValue:UniqueIntSequence,instructionsThatMustBeUndone:SeqUpdate):SeqUpdateRollBackToCheckpoint = {
    new SeqUpdateRollBackToCheckpoint(checkpointValue, instructionsThatMustBeUndone)
  }

  def unapply(u:SeqUpdateRollBackToCheckpoint):Option[UniqueIntSequence] = Some(u.checkpointValue)
}

class SeqUpdateRollBackToCheckpoint(val checkpointValue:UniqueIntSequence,instructionsThatMustBeUndone:SeqUpdate) extends SeqUpdate(checkpointValue){
  override protected[computation] def regularize : SeqUpdate = this

  override protected[computation] def reverse(target : UniqueIntSequence, newPrev:SeqUpdate) : SeqUpdate = {
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
 *
 */
trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate)
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

  override def defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean){
    super.defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean)
  }

  override def rollbackToCurrentCheckpoint(checkpoint:UniqueIntSequence) =
    super.rollbackToCurrentCheckpoint(checkpoint)

  override def releaseCurrentCheckpointAtCheckpoint(){
    super.releaseCurrentCheckpointAtCheckpoint()
  }

  def <==(i: SeqValue) {IdentitySeq(this,i)}

  def createClone:CBLSSeqVar = {
    val clone = new CBLSSeqVar(model,this.value,this.maxValue,"clone_of_" + this.name,maxPivot)
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

abstract class ChangingSeqValue(initialValue: Iterable[Int], val maxValue: Int, maxPivot: Int)
  extends AbstractVariable with SeqValue{

  //This section of code is for maintining the checkpoint stack.
  //stack does not include top checkpoint
  private var checkpointStackNotTop:List[(UniqueIntSequence,SeqUpdate,Boolean)] = List.empty
  private var topCheckpointIsActive:Boolean = false
  private var topCheckpointIsActiveDeactivated:Boolean = false
  private var topCheckpoint:UniqueIntSequence = null //can be null if no checkpoint
  private var notifiedSinceTopCheckpoint:SeqUpdate = null //what has been done after the current checkpoint (not maintained if checkpoint is not active)

  private def recordNotifiedChangesForCheckpoint(toNotify:SeqUpdate){
    if(topCheckpoint==null) return
    //includes checkpoint declaration, so we have to browse through them when concatenating the sequences
    toNotify match{
      case SeqUpdateLastNotified(value:UniqueIntSequence) =>
        //nothing to do :-)
        require(notifiedSinceTopCheckpoint.newValue quickEquals value)
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

      case SeqUpdateInsert(value,position,prev:SeqUpdate) =>
        notifiedSinceTopCheckpoint = SeqUpdateInsert(value,position,notifiedSinceTopCheckpoint)
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        notifiedSinceTopCheckpoint = SeqUpdateMove(fromIncluded,toIncluded,after,flip,notifiedSinceTopCheckpoint)
      case SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        notifiedSinceTopCheckpoint = SeqUpdateRemoveValue(value,notifiedSinceTopCheckpoint)
      case SeqUpdateRollBackToCheckpoint(checkpoint:UniqueIntSequence) =>
        require(checkpoint quickEquals topCheckpoint)
        notifiedSinceTopCheckpoint = SeqUpdateLastNotified(checkpoint)
      case SeqUpdateSet(value:UniqueIntSequence) =>
        notifiedSinceTopCheckpoint = SeqUpdateSet(value)
    }
  }

  private def popTopCheckpoint(checkpoint:UniqueIntSequence){
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

  private def instructionsToRollBackToTopCheckpoint(checkpoint:UniqueIntSequence, startingFrom:UniqueIntSequence):SeqUpdate = {
    require(topCheckpoint!=null)
    require(startingFrom quickEquals notifiedSinceTopCheckpoint.newValue)
    notifiedSinceTopCheckpoint.reverse(checkpoint)
  }

  //end of the checkpoint stack stuff


  private var mOldValue:UniqueIntSequence = UniqueIntSequence(initialValue)
  protected[computation] var toNotify:SeqUpdate = SeqUpdateSet(mOldValue)

  override def domain : Domain = 0 to maxValue
  override def max : Int = maxValue
  override def min : Int = 0

  override def value: UniqueIntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    model.propagate(this)
    mOldValue
  }

  def newValue:UniqueIntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    toNotify.newValue
  }

  override def toString:String = name + ":=" + (if(model.propagateOnToString) value else toNotify.newValue)

  def toStringNoPropagate: String = name + ":=" + toNotify.newValue.toString()

  //TODO: -1 for first position
  protected def insertAtPosition(value:Int,pos:Int){
    assert(pos < toNotify.newValue.size)
    assert(pos >= 0)
    toNotify = SeqUpdateInsert(value,pos,toNotify)
    notifyChanged()
  }

  protected def removeValue(value:Int){
    require(toNotify.newValue contains value)
    toNotify = SeqUpdateRemoveValue(value,toNotify)
    notifyChanged()
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
    notifyChanged()
  }

  protected [computation] def setValue(seq:UniqueIntSequence){
    //since we will override and lose the content of the toNotify, we have to ensure that there is no checkpoint delcared in this before
    pushCheckPoints(toNotify,false)
    toNotify = SeqUpdateSet(value)
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
      case x@SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        if(outputNeeded) SeqUpdateRemoveValue(value,pushCheckPoints(prev,true),x.newValue)
        else pushCheckPoints(prev,false)
      case SeqUpdateSet(value:UniqueIntSequence) =>
        updates
      case SeqUpdateLastNotified(value:UniqueIntSequence) =>
        updates
      case x@SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActiveCheckpoint:Boolean) =>
        //We have to push a checkpoint here!
        val updatesSincePrevCheckpoint = pushCheckPoints(prev,true)

        recordNotifiedChangesForCheckpoint(updatesSincePrevCheckpoint)
        recordNotifiedChangesForCheckpoint(SeqUpdateDefineCheckpoint(SeqUpdateLastNotified(updatesSincePrevCheckpoint.newValue),isActiveCheckpoint))

        require(updates.newValue quickEquals updatesSincePrevCheckpoint.newValue)
        SeqUpdateLastNotified(updates.newValue)
      case SeqUpdateRollBackToCheckpoint(checkpointValue:UniqueIntSequence) =>
        //must be the top checkpoint of the stack!
        require(checkpointValue quickEquals topCheckpoint)
        SeqUpdateLastNotified(checkpointValue)
    }
  }

  protected def defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean) = {
    toNotify = SeqUpdateDefineCheckpoint(toNotify.regularize,checkPointIsActive)
    notifyChanged()
  }

  protected def rollbackToCurrentCheckpoint(checkpoint:UniqueIntSequence) = {
    //check that the checkpoint is declared in the toNotify, actually.

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
  }

  /**
   * @param updates
   * @param checkpoint
   * @return the cleaned sequence, null if cleaning the sequence could not rollback to the checkpoint
   */
  private def popToNotifyUntilCheckpointDeclaration(updates:SeqUpdate,checkpoint:UniqueIntSequence):SeqUpdate = {
    updates match{
      case x@SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
      case x@SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
      case x@SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,checkpoint)
      case SeqUpdateSet(value:UniqueIntSequence) =>
        if(value quickEquals checkpoint){
          updates
        }else{
          null
        }
      case SeqUpdateLastNotified(value:UniqueIntSequence) =>
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
      case SeqUpdateRollBackToCheckpoint(checkpointValue:UniqueIntSequence) =>
        require(checkpointValue quickEquals checkpoint)
        updates
    }
  }

  protected def releaseCurrentCheckpointAtCheckpoint(){

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
  protected[computation] def privatePerformSeqPropagation(stableCheckpoint:Boolean): UniqueIntSequence = {
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next

    if(stableCheckpoint || !topCheckpointIsActive) toNotify = toNotify.regularize

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
    val start = SeqUpdateSet(theNewValue)
    mOldValue = theNewValue
    toNotify = start

    theNewValue
  }

  protected def :=(seq:UniqueIntSequence){
    setValue(seq)
    notifyChanged()
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

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
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
      case SeqUpdateLastNotified(value:UniqueIntSequence) =>
      //nothing to do here
      case SeqUpdateRollBackToCheckpoint(value:UniqueIntSequence) =>
        toValue.rollbackToCurrentCheckpoint(value)
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,activeCheckpoint:Boolean) =>
        digestChanges(prev)
        toValue.defineCurrentValueAsCheckpoint(activeCheckpoint)
    }
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value equals fromValue.value)
  }
}
