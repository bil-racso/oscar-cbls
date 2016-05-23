package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.fun.{LinearTransform, PiecewiseLinearBijectionNaive, Pivot}
import oscar.cbls.invariants.core.algo.lazyIt.LazyFilter
import oscar.cbls.invariants.core.algo.rb.{RBPosition, RedBlackTree}

import scala.language.implicitConversions

object UniqueIntSequence{
  def apply(values:Iterable[Int], maxPivot:Int = 10, maxSize:Int = 1000):UniqueIntSequence = {
    var toReturn = empty(maxPivot, maxSize)
    for (i <- values) {
      toReturn = toReturn.insertAtPosition(i, toReturn.size, false, false)
    }
    toReturn
  }

  def empty(maxPivot:Int = 10, maxSize:Int = 1000):UniqueIntSequence = new ConcreteUniqueIntSequence(
    RedBlackTree.empty[Int],
    RedBlackTree.empty[Int],
    PiecewiseLinearBijectionNaive.identity,
    0,
    maxPivot,
    maxSize
  )


  implicit def toIterable(seq:UniqueIntSequence):IterableUniqueIntSequence = new IterableUniqueIntSequence(seq)

  private var nextUniqueID:Int = Int.MinValue
  def getNewUniqueID():Int = {
    nextUniqueID +=1
    nextUniqueID
  }
}

class IterableUniqueIntSequence(sequence:UniqueIntSequence) extends Iterable[Int]{
  override def iterator : Iterator[Int] = sequence.iterator

  override def head : Int = sequence.valueAtPosition(0).head

  override def headOption : Option[Int] = sequence.valueAtPosition(0)

  override def last : Int = sequence.valueAtPosition(sequence.size-1).head

  override def lastOption : Option[Int] = sequence.valueAtPosition(sequence.size-1)
}

abstract class UniqueIntSequence(protected[seq] val uniqueID:Int = UniqueIntSequence.getNewUniqueID()) {

  def size : Int
  def isEmpty:Boolean
  def iterator : Iterator[Int] = new UniqueIntSequenceIterator(this)
  def iterable : Iterable[Int] = new IterableUniqueIntSequence(this)
  def content:Iterable[Int]
  //  def largestValue:Option[Int]
  //  def smallestValue:Option[Int]

  def valueAtPosition(position : Int) : Option[Int]
  def positionOfValue(value : Int) : Option[Int]
  def contains(value:Int):Boolean

  def explorerAtPosition(position : Int) : Option[IntSequenceExplorer]
  def explorerAtValue(value: Int) : Option[IntSequenceExplorer] =  {
    positionOfValue(value) match{
      case None => None
      case Some(pos) => explorerAtPosition(pos)
    }
  }

  def insertAtPosition(value:Int, pos:Int, fast:Boolean = false, autoRework:Boolean = true):UniqueIntSequence
  def delete(pos:Int, fast:Boolean=false,autoRework:Boolean = false):UniqueIntSequence
  def moveAfter(startPositionIncluded:Int, endPositionIncluded:Int, moveAfterPosition:Int, flip:Boolean, fast:Boolean = false, autoRework:Boolean = true):UniqueIntSequence

  def regularize(targetUniqueID:Int = this.uniqueID):ConcreteUniqueIntSequence
  def comitPendingMoves:UniqueIntSequence

  def check{}

  def quickEquals(that:UniqueIntSequence):Boolean = that != null && this.uniqueID == that.uniqueID
  def equals(that:UniqueIntSequence):Boolean = {
    quickEquals(that) || (that != null && (this.iterable equals that.iterable))
  }

  override def toString : String = {
    "UniqueIntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]      impl:" + descriptorString
  }

  def descriptorString : String
}

class ConcreteUniqueIntSequence(private[seq] val internalPositionToValue:RedBlackTree[Int],
                                private[seq] val valueToInternalPosition:RedBlackTree[Int],
                                private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                                private[seq] val startFreeRangeForInternalPosition:Int,
                                maxPivot:Int = 10, maxSize:Int = 1000, uniqueID:Int = UniqueIntSequence.getNewUniqueID()) extends UniqueIntSequence(uniqueID){

  override def descriptorString : String = "[" + this.content.mkString(",") + "]"

  override def toString : String = {
    "UniqueIntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]"
  }

  override def check {
    require(internalPositionToValue.content.sortBy(_._1) equals valueToInternalPosition.content.map({case (a,b) => (b,a)}).sortBy(_._1))
  }

  def size : Int = valueToInternalPosition.size
  def isEmpty:Boolean = internalPositionToValue.isEmpty

  def largestValue:Option[Int] = valueToInternalPosition.biggest match{case None => None case Some((k,_)) => Some(k)}
  def smallestValue:Option[Int] = valueToInternalPosition.smallest match{case None => None case Some((k,_)) => Some(k)}
  def contains(value:Int):Boolean = valueToInternalPosition.contains(value)

  def valueAtPosition(position : Int) : Option[Int] = {
    val internalPosition:Int = externalToInternalPosition.forward(position)
    internalPositionToValue.get(internalPosition)
  }

  def positionOfValue(value : Int) : Option[Int] = {
    valueToInternalPosition.get(value) match{
      case None => None
      case Some(internalPosition) => Some(externalToInternalPosition.backward(internalPosition))
    }
  }

  def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.forward.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition:Option[RBPosition[Pivot]],internalPosition) = currentPivotPosition match {
        case None => (externalToInternalPosition.forward.firstPivotAndPosition,position)
        case Some(p) => (p.next,p.value.f(position))
      }

      Some(new ConcreteIntSequenceExplorer(this,
        position,
        internalPositionToValue.positionOf(internalPosition).head,
        currentPivotPosition,
        pivotAbovePosition)()
      )
    }
  }

  def insertAtPosition(value:Int, pos:Int, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {

    //println(this + ".insertAtPosition(value:" + value + " pos:" + pos + ")")
    require(pos<=size,"inserting past the end of the sequence (size:" + size + " pos:" + pos + ")")

    if(fast) return new InsertedUniqueIntSequence(this,value,pos)

    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition,value)
    val newValueToInternalPosition = valueToInternalPosition.insert(value,startFreeRangeForInternalPosition)

    //move sequence after position, one upward
    //move inserted point at its position
    val oldExternalPosRelatedToFreeInternalPos = externalToInternalPosition.backward(startFreeRangeForInternalPosition)

    val newExternalToInternalPosition = if(pos == size) {
      //inserting at end of the sequence
      externalToInternalPosition.updateBefore(
        (size,size,LinearTransform(oldExternalPosRelatedToFreeInternalPos-pos,false)))
      //TODO: this migfht be always identity, actually, so useless!
    }else{
      //inserting somewhere within the sequence, need to shift upper part
      externalToInternalPosition.updateBefore(
        (pos+1,size,LinearTransform(-1,false)),
        (pos,pos,LinearTransform(oldExternalPosRelatedToFreeInternalPos-pos,false)))
    }

    new ConcreteUniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1,
      maxPivot,maxSize)
  }

  def delete(pos:Int, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {
    //println(this + ".delete(pos:" + pos + ")")
    require(pos<size,"deleting past the end of the sequence (size:" + size + " pos:" + pos + ")")
    require(pos>=0,"deleting at negative pos:" + pos)

    if(fast) return new DeletedUniqueIntSequence(this,pos)

    val internalPosition = externalToInternalPosition(pos)
    val value = internalPositionToValue.get(internalPosition).head
    val largestInternalPosition = startFreeRangeForInternalPosition-1

    val valueAtLargestInternalPosition = internalPositionToValue.get(largestInternalPosition).head

    val newInternalPositionToValue = internalPositionToValue.
      insert(internalPosition,valueAtLargestInternalPosition).
      remove(largestInternalPosition)

    val newValueToInternalPosition = valueToInternalPosition.
      insert(valueAtLargestInternalPosition,internalPosition).
      remove(value)

    //now, update the fct knowing the move and remove
    val externalPositionAssociatedToLargestInternalPosition = externalToInternalPosition.backward(largestInternalPosition)

    val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
      (externalPositionAssociatedToLargestInternalPosition,
        externalPositionAssociatedToLargestInternalPosition,
        LinearTransform(pos-externalPositionAssociatedToLargestInternalPosition,false)),
      (pos,pos,LinearTransform(externalPositionAssociatedToLargestInternalPosition - pos,false))).updateBefore(
      (pos,size-2,LinearTransform(1,false)),(size-1,size-1,LinearTransform(pos - size + 1,false)))

    new ConcreteUniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition - 1,
      maxPivot,maxSize)
  }

  def moveAfter(startPositionIncluded:Int, endPositionIncluded:Int, moveAfterPosition:Int, flip:Boolean, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {
    //println(this + ".moveAfter(startPositionIncluded:" + startPositionIncluded + " endPositionIncluded:" + endPositionIncluded + " moveAfterPosition:" + moveAfterPosition + " flip:" + flip + ")")
    require(
      moveAfterPosition < startPositionIncluded || moveAfterPosition>endPositionIncluded,
      "moveAfterPosition=" +  moveAfterPosition + " cannot be between startPositionIncluded=" + startPositionIncluded + " and endPositionIncluded=" + endPositionIncluded)
    require(startPositionIncluded <= endPositionIncluded, "startPositionIncluded=" + startPositionIncluded + " should be <= endPositionIncluded=" + endPositionIncluded)

    if(fast) return new MovedUniqueIntSequence(this,startPositionIncluded,endPositionIncluded,moveAfterPosition,flip)

    if(moveAfterPosition + 1 == startPositionIncluded) {
      //not moving
      if(flip) { //just flipping
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded,endPositionIncluded,LinearTransform(endPositionIncluded + startPositionIncluded,true)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition,
          maxPivot,
          maxSize)

      }else{
        this //nop
      }
    }else{
      if(moveAfterPosition > startPositionIncluded){ //move upwards
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded,
            moveAfterPosition + startPositionIncluded - endPositionIncluded -1,
            LinearTransform(endPositionIncluded + 1 - startPositionIncluded,false)),
          (startPositionIncluded + moveAfterPosition - endPositionIncluded,
            moveAfterPosition,
            LinearTransform(if(flip) startPositionIncluded + moveAfterPosition
            else endPositionIncluded - moveAfterPosition,flip)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition,
          maxPivot,
          maxSize)

      }else{ //move downwards
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (moveAfterPosition+1,
            moveAfterPosition + endPositionIncluded  - startPositionIncluded + 1,
            LinearTransform(if(flip) endPositionIncluded + moveAfterPosition + 1 else startPositionIncluded - moveAfterPosition - 1,flip)),
          (moveAfterPosition + endPositionIncluded  - startPositionIncluded + 2,
            endPositionIncluded,
            LinearTransform(startPositionIncluded - endPositionIncluded - 1,false)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition,
          maxPivot,
          maxSize)
      }
    }
  }

  def regularize(targetUniqueID:Int = this.uniqueID):ConcreteUniqueIntSequence = {
    //println("regularize")
    var explorer = this.explorerAtPosition(0)
    var newInternalPositionToValues = RedBlackTree.empty[Int]
    var newValueToInternalPosition = RedBlackTree.empty[Int]
    while(explorer match{
      case None => false
      case Some(position) =>
        newInternalPositionToValues = newInternalPositionToValues.insert(position.position,position.value)
        newValueToInternalPosition = newValueToInternalPosition.insert(position.value,position.position)
        explorer = position.next
        true
    }){}

    new ConcreteUniqueIntSequence(newInternalPositionToValues,
      newValueToInternalPosition,
      PiecewiseLinearBijectionNaive.identity,
      newInternalPositionToValues.size,
      maxPivot, maxSize,targetUniqueID)
  }

  override def comitPendingMoves : UniqueIntSequence = this

  override def content : Iterable[Int] = internalPositionToValue.values
}

class UniqueIntSequenceIterator(s:UniqueIntSequence) extends Iterator[Int] {
  var crawler = s.explorerAtPosition(0)

  override def hasNext : Boolean =
    crawler match{
      case None => false
      case Some(_) => true}

  override def next() : Int = {
    val position = crawler.head
    crawler = position.next
    position.value
  }
}

abstract class IntSequenceExplorer{
  def value:Int
  def position:Int
  def next:Option[IntSequenceExplorer]
  def prev:Option[IntSequenceExplorer]
}


class ConcreteIntSequenceExplorer(sequence:ConcreteUniqueIntSequence,
                                  override val position:Int,
                                  positionInRB:RBPosition[Int],
                                  currentPivotPosition:Option[RBPosition[Pivot]],
                                  pivotAbovePosition:Option[RBPosition[Pivot]])(
                                   limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                                     case None => Int.MaxValue
                                     case Some(p) => p.value.fromValue-1},
                                   limitBelowForCurrentPivot:Int = currentPivotPosition match{
                                     case None => Int.MinValue
                                     case Some(p) => p.value.fromValue},
                                   slopeIsPositive:Boolean = currentPivotPosition match{
                                     case None => true
                                     case Some(p) => !p.value.f.minus}
                                   ) extends IntSequenceExplorer{

  override def toString : String = "ConcreteIntSequenceExplorer(position:" + position + " value:" + value + " currentPivotPosition:" + currentPivotPosition + " pivotAbovePosition:" + pivotAbovePosition + " positionInRB:" + positionInRB + ")"

  override val value : Int = positionInRB.value

  override def next : Option[IntSequenceExplorer] = {
    if(position == sequence.size-1) return None
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head
      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInRBOpt = sequence.internalPositionToValue.positionOf(pivotAbovePosition.head.value.f(newPosition))
      newPositionInRBOpt match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new ConcreteIntSequenceExplorer(sequence,
            newPosition,
            newPositionInRB,
            pivotAbovePosition,
            newPivotAbovePosition)(limitBelowForCurrentPivot = newPosition))
      }
    }else{
      //do not change pivot

      (if(slopeIsPositive) positionInRB.next else positionInRB.prev) match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new ConcreteIntSequenceExplorer(sequence,
            position + 1,
            newPositionInRB,
            currentPivotPosition,
            pivotAbovePosition)(
            limitAboveForCurrentPivot,
            limitBelowForCurrentPivot,
            slopeIsPositive))
      }
    }
  }

  override def prev : Option[IntSequenceExplorer] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot

      val newPosition = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition match{case None => newPosition case Some(position2) => position2.value.f(newPosition)}
      val newCurrentPositionInRB = sequence.internalPositionToValue.positionOf(newInternalPosition).head
      //println("change pivot newPosition:" + newPosition + " newCurrentPivotPosition:" + newCurrentPivotPosition + " oldPosition:" + currentPivotPosition)
      Some(new ConcreteIntSequenceExplorer(sequence,
        newPosition,
        newCurrentPositionInRB,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      //println("not change pivot")
      Some(new ConcreteIntSequenceExplorer(sequence,
        position-1,
        if(slopeIsPositive) positionInRB.prev.head else positionInRB.next.head,
        currentPivotPosition,
        pivotAbovePosition)(
        limitAboveForCurrentPivot,
        limitBelowForCurrentPivot,
        slopeIsPositive))
    }
  }
}

abstract class StackedUpdateUniqueIntSequence extends UniqueIntSequence(){
  override def delete(pos : Int, fast:Boolean,autoRework:Boolean) : UniqueIntSequence = new DeletedUniqueIntSequence(this,pos)

  override def moveAfter(startPositionIncluded : Int, endPositionIncluded : Int, moveAfterPosition : Int, flip : Boolean, fast:Boolean,autoRework:Boolean) : UniqueIntSequence =
    new MovedUniqueIntSequence(this,startPositionIncluded,endPositionIncluded,moveAfterPosition,flip)

  override def insertAtPosition(value : Int, pos : Int, fast:Boolean,autoRework:Boolean) : UniqueIntSequence =
    new InsertedUniqueIntSequence(this,value:Int,pos:Int)

  override def regularize(targetUniqueID:Int = this.uniqueID) : ConcreteUniqueIntSequence = comitPendingMoves.regularize(targetUniqueID)
}

class MovedUniqueIntSequence(val seq:UniqueIntSequence,
                             startPositionIncluded:Int,
                             endPositionIncluded:Int,
                             moveAfterPosition:Int,
                             flip:Boolean)
  extends StackedUpdateUniqueIntSequence{

  override def content : Iterable[Int] = seq.content

  override def descriptorString : String = seq.descriptorString + ".moved(startPos:" + startPositionIncluded + " endPos:" + endPositionIncluded + " targetPos:" + moveAfterPosition + " flip:" + flip + ")"

  val localBijection =
    if(moveAfterPosition + 1 == startPositionIncluded) {
      //not moving
      if(flip) { //just flipping
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (startPositionIncluded,endPositionIncluded,LinearTransform(endPositionIncluded + startPositionIncluded,true)))
      }else{
        PiecewiseLinearBijectionNaive.identity
      }
    }else {
      if (moveAfterPosition > startPositionIncluded) {
        //move upwards
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (startPositionIncluded, moveAfterPosition + startPositionIncluded - endPositionIncluded - 1,
            LinearTransform(endPositionIncluded + 1 - startPositionIncluded, false)),
          (startPositionIncluded + moveAfterPosition - endPositionIncluded,moveAfterPosition,
            LinearTransform(if (flip) startPositionIncluded + moveAfterPosition
            else endPositionIncluded - moveAfterPosition, flip)))
      } else {
        //move downwards
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (moveAfterPosition + 1, moveAfterPosition + endPositionIncluded - startPositionIncluded + 1,
            LinearTransform(if (flip) endPositionIncluded + moveAfterPosition + 1 else startPositionIncluded - moveAfterPosition - 1, flip)),
          (moveAfterPosition + endPositionIncluded - startPositionIncluded + 2, endPositionIncluded,
            LinearTransform(startPositionIncluded - endPositionIncluded - 1, false)))
      }
    }

  override def size : Int = seq.size

  override def comitPendingMoves:UniqueIntSequence = seq.comitPendingMoves.moveAfter(startPositionIncluded,endPositionIncluded,moveAfterPosition,flip,fast=false,autoRework = false)

  override def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    val positionOfCurrentPivot = localBijection.forward.pivotWithPositionApplyingTo(position)
    seq.explorerAtPosition(localBijection.forward(position)) match{
      case None => None
      case Some(explorerInBasicSequence) =>
        Some(new MovedIntSequenceExplorer(this,position,
          explorerInBasicSequence,
          positionOfCurrentPivot,
          positionOfCurrentPivot match{case None => localBijection.forward.firstPivotAndPosition case Some(x) => x.next})())
    }
  }

  override def positionOfValue(value : Int) : Option[Int] = {
    seq.positionOfValue(value) match{
      case None => None
      case Some(intermediary) => Some(localBijection.backward(intermediary))
    }
  }

  override def contains(value : Int) : Boolean = seq.contains(value)

  override def isEmpty : Boolean = seq.isEmpty

  override def valueAtPosition(position : Int) : Option[Int] = {
    seq.valueAtPosition(localBijection.forward(position))
  }
}

class MovedIntSequenceExplorer(sequence:MovedUniqueIntSequence,
                               override val position:Int,
                               positionInBasicSequence:IntSequenceExplorer,
                               currentPivotPosition:Option[RBPosition[Pivot]],
                               pivotAbovePosition:Option[RBPosition[Pivot]])(
                                limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                                  case None => Int.MaxValue
                                  case Some(p) => p.value.fromValue-1},
                                limitBelowForCurrentPivot:Int = currentPivotPosition match{
                                  case None => Int.MinValue
                                  case Some(p) => p.value.fromValue},
                                slopeIsPositive:Boolean = currentPivotPosition match{
                                  case None => true
                                  case Some(p) => !p.value.f.minus}
                                ) extends IntSequenceExplorer{

  //  override def toString : String = "MovedIntSequenceExplorer(position:" + position + " value:" + value + " currentPivotPosition:" + currentPivotPosition + " pivotAbovePosition:" +
  //    pivotAbovePosition + " basicPositionn:" + positionInBasicSequence + ")"

  override val value : Int = positionInBasicSequence.value

  override def next : Option[IntSequenceExplorer] = {
    if(position == sequence.size-1) return None
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head
      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInBasicSequence = sequence.seq.explorerAtPosition(pivotAbovePosition.head.value.f(newPosition))
      newPositionInBasicSequence match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new MovedIntSequenceExplorer(sequence,
            newPosition,
            newPositionInRB,
            pivotAbovePosition,
            newPivotAbovePosition)(limitBelowForCurrentPivot = newPosition))
      }
    }else{
      //do not change pivot

      (if(slopeIsPositive) positionInBasicSequence.next else positionInBasicSequence.prev) match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new MovedIntSequenceExplorer(sequence,
            position + 1,
            newPositionInRB,
            currentPivotPosition,
            pivotAbovePosition)(
            limitAboveForCurrentPivot,
            limitBelowForCurrentPivot,
            slopeIsPositive))
      }
    }
  }

  override def prev : Option[IntSequenceExplorer] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot

      val newPosition = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition match{case None => newPosition case Some(position2) => position2.value.f(newPosition)}

      //println("change pivot newPosition:" + newPosition + " newCurrentPivotPosition:" + newCurrentPivotPosition + " oldPosition:" + currentPivotPosition)
      Some(new MovedIntSequenceExplorer(sequence,
        newPosition,
        sequence.seq.explorerAtPosition(newInternalPosition).head,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      //println("not change pivot")
      Some(new MovedIntSequenceExplorer(sequence,
        position-1,
        if(slopeIsPositive) positionInBasicSequence.prev.head else positionInBasicSequence.next.head,
        currentPivotPosition,
        pivotAbovePosition)(
        limitAboveForCurrentPivot,
        limitBelowForCurrentPivot,
        slopeIsPositive))
    }
  }
}

class InsertedUniqueIntSequence(seq:UniqueIntSequence,
                                val value:Int,
                                val pos:Int)
  extends StackedUpdateUniqueIntSequence {
  override def size : Int = seq.size + 1

  override def descriptorString : String = seq.descriptorString + ".inserted(val:" + value + " pos:" + pos + ")"

  override def content : Iterable[Int] = seq.content ++ List(value)

  override def positionOfValue(value : Int) : Option[Int] = {
    seq.positionOfValue(value) match {
      case None => if (value == this.value) Some(pos) else None
      case Some(p) => if (p < pos) Some(p) else Some(p + 1)
    }
  }

  override def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    if (position == this.pos) {
      if (position == 0) {
        Some(new InsertedIntSequenceExplorer(this, position, seq.explorerAtPosition(0), true, true))
      } else {
        Some(new InsertedIntSequenceExplorer(this, position, seq.explorerAtPosition(position - 1), true, false))
      }
    } else if (position < this.pos) {
      seq.explorerAtPosition(position) match{
        case None => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    } else {
      seq.explorerAtPosition(position-1) match{
        case None => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    }
  }

  override def contains(value : Int) : Boolean = value == this.value || seq.contains(value)

  override def comitPendingMoves : UniqueIntSequence = seq.comitPendingMoves.insertAtPosition(value, pos, fast = false, autoRework = false)

  override def isEmpty : Boolean = false

  override def valueAtPosition(position : Int) : Option[Int] = {
    if (position == pos) Some(value)
    else if (position < pos) seq.valueAtPosition(position)
    else seq.valueAtPosition(position - 1)
  }
}


class InsertedIntSequenceExplorer(seq:InsertedUniqueIntSequence,
                                  val position:Int,
                                  explorerInOriginalSeq:Option[IntSequenceExplorer],
                                  atInsertedValue:Boolean,
                                  originalExplorerIsAbove:Boolean)
  extends IntSequenceExplorer {
  override def value : Int = if(atInsertedValue) seq.value else explorerInOriginalSeq.head.value

  override def next : Option[IntSequenceExplorer] = {
    if (atInsertedValue) {
      //we are leaving the inserted position
      explorerInOriginalSeq match{
        case None => None
        case Some(p) =>
          if (originalExplorerIsAbove) Some(new InsertedIntSequenceExplorer(seq, position + 1, explorerInOriginalSeq, atInsertedValue = false, originalExplorerIsAbove = false))
          else {
            p.next match {
              case None => None
              case Some(next1) =>
                Some(new InsertedIntSequenceExplorer(seq, position + 1, Some(next1), atInsertedValue = false, originalExplorerIsAbove = false))
            }
          }
      }
    }else {
      val nextPosition = position + 1
      if (nextPosition == seq.pos) {
        //we are getting into the inserted position
        Some(new InsertedIntSequenceExplorer(seq, position + 1, explorerInOriginalSeq, atInsertedValue = true, originalExplorerIsAbove = false))
      } else {
        //nothing special
        explorerInOriginalSeq.head.next match {
          case None => None
          case Some(next) => Some(new InsertedIntSequenceExplorer(seq, position + 1, Some(next), atInsertedValue = false, originalExplorerIsAbove = false))
        }
      }
    }
  }

  override def prev : Option[IntSequenceExplorer] = {
    if (atInsertedValue) {
      explorerInOriginalSeq match {
        case None => None
        case Some(p) =>
          //we are leaving the inserted position
          if (!originalExplorerIsAbove) Some(new InsertedIntSequenceExplorer(seq, position - 1, explorerInOriginalSeq, atInsertedValue = false, originalExplorerIsAbove = false))
          else {
            p.prev match {
              case None => None
              case Some(prev1) => Some(new InsertedIntSequenceExplorer(seq, position - 1, Some(prev1), atInsertedValue = false, originalExplorerIsAbove = false))
            }
          }
      }
    } else {
      val prevPosition = position - 1
      if (prevPosition == seq.pos) {
        //we are getting into the inserted position
        Some(new InsertedIntSequenceExplorer(seq, position - 1, explorerInOriginalSeq, atInsertedValue = true, originalExplorerIsAbove = true))
      } else {
        //nothing special
        explorerInOriginalSeq.head.prev match {
          case None => None
          case Some(prev) => Some(new InsertedIntSequenceExplorer(seq, position - 1, Some(prev), atInsertedValue = false, originalExplorerIsAbove = false))
        }
      }
    }
  }
}

class DeletedUniqueIntSequence(seq:UniqueIntSequence,
                               val position:Int)
  extends StackedUpdateUniqueIntSequence{

  val value = seq.valueAtPosition(position).head

  override def descriptorString : String = seq.descriptorString + ".deleted(pos:" + position + " val:" + value + ")"

  override def content : Iterable[Int] = new LazyFilter(seq.content,_ != value)

  override def size : Int = seq.size - 1

  override def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    seq.explorerAtPosition(if (position < this.position) position else position + 1) match {
      case None => None
      case Some(e) => Some(new DeletedIntSequenceExplorer(this, position, e))
    }
  }

  override def positionOfValue(value : Int) : Option[Int] = {
    if(value == this.value) None
    else seq.positionOfValue(value) match{
      case None => None
      case Some(p) => if(p < this.position) Some(p) else Some(p-1)
    }
  }

  override def contains(value : Int) : Boolean = value != this.value && seq.contains(value)

  override def comitPendingMoves : UniqueIntSequence = seq.comitPendingMoves.delete(this.position,fast=false,autoRework=false)

  override def isEmpty : Boolean = false

  override def valueAtPosition(position : Int) : Option[Int] = {
    if(position >= this.position) seq.valueAtPosition(position+1)
    else seq.valueAtPosition(position)
  }
}

class DeletedIntSequenceExplorer(seq:DeletedUniqueIntSequence,
                                 val position:Int,
                                 explorerInOriginalSeq:IntSequenceExplorer)
  extends IntSequenceExplorer{
  override def value : Int = explorerInOriginalSeq.value

  override def prev : Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.prev match {
      case None => None
      case Some(tentativePos) =>
        if(tentativePos.position == seq.position)
          tentativePos.prev match {
            case None => None
            case Some(secondTentativePos) => Some(new DeletedIntSequenceExplorer(seq, position - 1, secondTentativePos))
          }
        else Some(new DeletedIntSequenceExplorer(seq, position - 1, tentativePos))
    }
  }

  override def next : Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.next match {
      case None => None
      case Some(tentativePos) =>
        if(tentativePos.position == seq.position)
          tentativePos.next match {
            case None => None
            case Some(secondTentativePos) => Some(new DeletedIntSequenceExplorer(seq, position + 1, secondTentativePos))
          }
        else Some(new DeletedIntSequenceExplorer(seq, position + 1, tentativePos))
    }
  }
}

