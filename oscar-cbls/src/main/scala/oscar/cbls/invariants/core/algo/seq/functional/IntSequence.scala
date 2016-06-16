package oscar.cbls.invariants.core.algo.seq.functional
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

import oscar.cbls.invariants.core.algo.fun.{LinearTransform, PiecewiseLinearBijectionNaive, Pivot}
import oscar.cbls.invariants.core.algo.rb.{RBTMPosition, RedBlackTreeMap}

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

object IntSequence{
  def apply(values:Iterable[Int]):IntSequence = {
    var toReturn = empty()
    for (i <- values) {
      toReturn = toReturn.insertAtPosition(i, toReturn.size, false, false)
    }
    toReturn
  }

  def empty():IntSequence = new ConcreteIntSequence(
    RedBlackTreeMap.empty[Int],
    RedBlackTreeMap.empty[RedBlackTreeMap[Int]],
    PiecewiseLinearBijectionNaive.identity,
    0
  )

  implicit def toIterable(seq:IntSequence):IterableIntSequence = new IterableIntSequence(seq)

  private var nextUniqueID:Int = Int.MinValue
  def getNewUniqueID():Int = {
    nextUniqueID +=1
    nextUniqueID
  }
}

class IterableIntSequence(sequence:IntSequence) extends Iterable[Int]{
  override def iterator : Iterator[Int] = sequence.iterator

  override def head : Int = sequence.valueAtPosition(0).head

  override def headOption : Option[Int] = sequence.valueAtPosition(0)

  override def last : Int = sequence.valueAtPosition(sequence.size-1).head

  override def lastOption : Option[Int] = sequence.valueAtPosition(sequence.size-1)
}

abstract class IntSequence(protected[seq] val uniqueID:Int = IntSequence.getNewUniqueID()) {

  def size : Int

  def isEmpty : Boolean = size > 0

  def iterator : Iterator[Int] = new IntSequenceIterator(this.explorerAtPosition(0))

  def iterateFromAnyOcurrenceOfValue(value:Int):Iterator[Int] = new IntSequenceIterator(this.explorerAtAnyOccurrence(value))

  def iterable : Iterable[Int] = new IterableIntSequence(this)

  def nbOccurrence(value:Int):Int

  def unorderedContentNoDuplicate : List[Int]

  def valueAtPosition(position : Int) : Option[Int]

  def positionsOfValue(value : Int) : SortedSet[Int]

  def contains(value : Int) : Boolean

  def explorerAtPosition(position : Int) : Option[IntSequenceExplorer]

  def valuesBetweenPositions(fromPositionIncluded:Int,toPositionIncluded:Int):SortedSet[Int] = {
    var toReturn = SortedSet.empty[Int]
    var e = explorerAtPosition(fromPositionIncluded)
    while(e match{
      case None => false
      case Some(explorer) =>
        if (explorer.position <= toPositionIncluded){
          toReturn = toReturn + explorer.value
          e = explorer.next
          true
        }else false
    }){}
    toReturn
  }

  def explorerAtFirstOccurrence(value : Int) : Option[IntSequenceExplorer] = {
    positionOfFirstOccurrence(value : Int) match {
      case None => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def explorerAtLastOccurrence(value : Int) : Option[IntSequenceExplorer] = {
    positionOfLastOccurrence(value : Int) match {
      case None => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def explorerAtAnyOccurrence(value : Int) : Option[IntSequenceExplorer] = {
    positionOfAnyOccurrence(value : Int) match {
      case None => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def positionOfFirstOccurrence(value : Int) : Option[Int] = {
    positionsOfValue(value) match {
      case null => None
      case x => Some(x.toIterable.min)
    }
  }

  def positionOfLastOccurrence(value : Int) : Option[Int] = {
    positionsOfValue(value) match {
      case null => None
      case x => Some(x.toIterable.max)
    }
  }

  def positionOfAnyOccurrence(value:Int):Option[Int] = {
    positionsOfValue(value) match {
      case null => None
      case x => Some(x.head)
    }
  }

  def insertAtPosition(value:Int, pos:Int, fast:Boolean = false, autoRework:Boolean = true):IntSequence
  def delete(pos:Int, fast:Boolean=false,autoRework:Boolean = false):IntSequence
  def moveAfter(startPositionIncluded:Int, endPositionIncluded:Int, moveAfterPosition:Int, flip:Boolean, fast:Boolean = false, autoRework:Boolean = true):IntSequence

  def regularizeToMaxPivot(maxPivot: Int, targetUniqueID: Int = this.uniqueID) :ConcreteIntSequence

  def regularize(targetUniqueID:Int = this.uniqueID):ConcreteIntSequence
  def commitPendingMoves:IntSequence

  def check{}

  def quickEquals(that:IntSequence):Boolean = that != null && this.uniqueID == that.uniqueID
  def equals(that:IntSequence):Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString : String = {
    "IntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]_impl:" + descriptorString
  }

  def descriptorString : String

  def predecessorPos2Val(position:Int):Option[Int] = {
    valueAtPosition(position-1)
  }

  def successorPos2Val(position:Int):Option[Int] = {
    valueAtPosition(position+1)
  }
}

class ConcreteIntSequence(private[seq] val internalPositionToValue:RedBlackTreeMap[Int],
                          private[seq] val valueToInternalPositions:RedBlackTreeMap[RedBlackTreeMap[Int]],
                          private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                          private[seq] val startFreeRangeForInternalPosition:Int,
                          uniqueID:Int = IntSequence.getNewUniqueID()) extends IntSequence(uniqueID) {

  override def descriptorString : String = "[" + this.unorderedContentNoDuplicate.mkString(",") + "]"

  override def toString : String = {
    "UniqueIntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]_impl:concrete"
  }

  override def check {
    externalToInternalPosition.checkBijection()
    require(internalPositionToValue.content.sortBy(_._1) equals valueToInternalPositions.content.flatMap({case (a, b) => b.keys.map(x => (x, a))}).sortBy(_._1))
  }

  def size : Int = internalPositionToValue.size

  override def isEmpty : Boolean = internalPositionToValue.isEmpty

  override def nbOccurrence(value : Int) : Int = valueToInternalPositions.get(value) match {
    case None => 0
    case Some(p) => p.size
  }

  def largestValue : Option[Int] = valueToInternalPositions.biggest match {
    case None => None
    case Some((k, _)) => Some(k)
  }

  def smallestValue : Option[Int] = valueToInternalPositions.smallest match {
    case None => None
    case Some((k, _)) => Some(k)
  }

  def contains(value : Int) : Boolean = valueToInternalPositions.contains(value)

  def valueAtPosition(position : Int) : Option[Int] = {
    val internalPosition : Int = externalToInternalPosition.forward(position)
    internalPositionToValue.get(internalPosition)
  }

  def positionsOfValue(value : Int) : SortedSet[Int] = {
    valueToInternalPositions.get(value) match {
      case None => SortedSet.empty
      case Some(internalPositions) =>
        SortedSet.empty ++ internalPositions.values.map(externalToInternalPosition.backward(_))
    }
  }

  def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.forward.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition : Option[RBTMPosition[Pivot]], internalPosition) = currentPivotPosition match {
        case None => (externalToInternalPosition.forward.firstPivotAndPosition, position)
        case Some(p) => (p.next, p.value.f(position))
      }

      Some(new ConcreteIntSequenceExplorer(this,
        position,
        internalPositionToValue.positionOf(internalPosition).head,
        currentPivotPosition,
        pivotAbovePosition)()
      )
    }
  }

  private def internalInsertToValueToInternalPositions(value : Int, internalPosition : Int, valueToInternalPositions : RedBlackTreeMap[RedBlackTreeMap[Int]]) : RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      case None => valueToInternalPositions.insert(value, RedBlackTreeMap(List((internalPosition, internalPosition))))
      case Some(l) => valueToInternalPositions.insert(value, l.insert(internalPosition, internalPosition))
    }
  }

  private def internalRemoveFromValueToInternalPositions(value : Int, internalPosition : Int,
                                                         valueToInternalPositions : RedBlackTreeMap[RedBlackTreeMap[Int]])
  : RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      case None => valueToInternalPositions
      case Some(l) =>
        val newSet = l.remove(internalPosition)
        if (newSet.isEmpty) valueToInternalPositions.remove(value)
        else valueToInternalPositions.insert(value, newSet)
    }
  }

  def insertAtPosition(value : Int, pos : Int, fast : Boolean, autoRework : Boolean) : IntSequence = {

    //println(this + ".insertAtPosition(value:" + value + " pos:" + pos + ")")
    require(pos <= size, "inserting past the end of the sequence (size:" + size + " pos:" + pos + ")")

    if (fast) return new InsertedIntSequence(this, value, pos)

    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition, value)
    val newValueToInternalPosition = internalInsertToValueToInternalPositions(value, startFreeRangeForInternalPosition, valueToInternalPositions)

    //move sequence after position, one upward
    //move inserted point at its position
    val oldExternalPosRelatedToFreeInternalPos = externalToInternalPosition.backward(startFreeRangeForInternalPosition)

    val newExternalToInternalPosition = if (pos == size) {
      //inserting at end of the sequence
      externalToInternalPosition.updateBefore(
        (size, size, LinearTransform(oldExternalPosRelatedToFreeInternalPos - pos, false)))
      //TODO: this migfht be always identity, actually, so useless!
    } else {
      //inserting somewhere within the sequence, need to shift upper part
      externalToInternalPosition.updateBefore(
        (pos + 1, size, LinearTransform(-1, false)),
        (pos, pos, LinearTransform(oldExternalPosRelatedToFreeInternalPos - pos, false)))
    }

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition + 1)
  }

  def delete(pos : Int, fast : Boolean, autoRework : Boolean) : IntSequence = {
    //println(this + ".delete(pos:" + pos + ")")
    require(pos < size, "deleting past the end of the sequence (size:" + size + " pos:" + pos + ")")
    require(pos >= 0, "deleting at negative pos:" + pos)

    if (fast) return new RemovedIntSequence(this, pos)

    val internalPosition = externalToInternalPosition(pos)
    val value = internalPositionToValue.get(internalPosition).head
    val largestInternalPosition = startFreeRangeForInternalPosition - 1

    val valueAtLargestInternalPosition : Int = internalPositionToValue.get(largestInternalPosition).head

    val newInternalPositionToValue = internalPositionToValue.
      insert(internalPosition, valueAtLargestInternalPosition).
      remove(largestInternalPosition)

    val newValueToInternalPositions =
      internalInsertToValueToInternalPositions(valueAtLargestInternalPosition, internalPosition,
        internalRemoveFromValueToInternalPositions(value, largestInternalPosition, valueToInternalPositions))

    //now, update the fct knowing the move and remove
    val externalPositionAssociatedToLargestInternalPosition = externalToInternalPosition.backward(largestInternalPosition)

    val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
      (externalPositionAssociatedToLargestInternalPosition,
        externalPositionAssociatedToLargestInternalPosition,
        LinearTransform(pos - externalPositionAssociatedToLargestInternalPosition, false)),
      (pos, pos, LinearTransform(externalPositionAssociatedToLargestInternalPosition - pos, false))).updateBefore(
      (pos, size - 2, LinearTransform(1, false)), (size - 1, size - 1, LinearTransform(pos - size + 1, false)))

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPositions,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition - 1)
  }

  def moveAfter(startPositionIncluded : Int, endPositionIncluded : Int, moveAfterPosition : Int, flip : Boolean, fast : Boolean, autoRework : Boolean) : IntSequence = {
    //println(this + ".moveAfter(startPositionIncluded:" + startPositionIncluded + " endPositionIncluded:" + endPositionIncluded + " moveAfterPosition:" + moveAfterPosition + " flip:" + flip + ")")
    require(startPositionIncluded >= 0 && startPositionIncluded < size, "startPositionIncluded should be in [0,size[ in UniqueIntSequence.moveAfter")
    require(endPositionIncluded >= 0 && endPositionIncluded < size, "endPositionIncluded should be in [0,size[ in UniqueIntSequence.moveAfter")
    require(moveAfterPosition >= -1 && moveAfterPosition < size, "moveAfterPosition=" + moveAfterPosition + " should be in [-1,size=" + size+"[ in UniqueIntSequence.moveAfter")

    require(
      moveAfterPosition < startPositionIncluded || moveAfterPosition > endPositionIncluded,
      "moveAfterPosition=" + moveAfterPosition + " cannot be between startPositionIncluded=" + startPositionIncluded + " and endPositionIncluded=" + endPositionIncluded)
    require(startPositionIncluded <= endPositionIncluded, "startPositionIncluded=" + startPositionIncluded + " should be <= endPositionIncluded=" + endPositionIncluded)

    if (fast) return new MovedIntSequence(this, startPositionIncluded, endPositionIncluded, moveAfterPosition, flip)

    if (moveAfterPosition + 1 == startPositionIncluded) {
      //not moving
      if (flip) {
        //just flipping
        val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded, endPositionIncluded, LinearTransform(endPositionIncluded + startPositionIncluded, true)))

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)

      } else {
        this //nop
      }
    } else {
      if (moveAfterPosition > startPositionIncluded) {
        //move upwards
        val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded,
            moveAfterPosition + startPositionIncluded - endPositionIncluded - 1,
            LinearTransform(endPositionIncluded + 1 - startPositionIncluded, false)),
          (startPositionIncluded + moveAfterPosition - endPositionIncluded,
            moveAfterPosition,
            LinearTransform(if (flip) startPositionIncluded + moveAfterPosition
            else endPositionIncluded - moveAfterPosition, flip)))

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)

      } else {
        //move downwards
        val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (moveAfterPosition + 1,
            moveAfterPosition + endPositionIncluded - startPositionIncluded + 1,
            LinearTransform(if (flip) endPositionIncluded + moveAfterPosition + 1 else startPositionIncluded - moveAfterPosition - 1, flip)),
          (moveAfterPosition + endPositionIncluded - startPositionIncluded + 2,
            endPositionIncluded,
            LinearTransform(startPositionIncluded - endPositionIncluded - 1, false)))

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)
      }
    }
  }

  override def regularizeToMaxPivot(maxPivot: Int, targetUniqueID: Int = this.uniqueID) :ConcreteIntSequence = {
    if(this.externalToInternalPosition.forward.nbPivot > maxPivot){
      regularize(targetUniqueID)
    }else{
      if (targetUniqueID != this.uniqueID){
        new ConcreteIntSequence(internalPositionToValue,
          valueToInternalPositions,
          externalToInternalPosition,
          size, targetUniqueID)
      }else this
    }
  }

  def regularize(targetUniqueID : Int = this.uniqueID) : ConcreteIntSequence = {
    //TODO: maybe we can go faster with newValueToInternalPositions?
    var explorer = this.explorerAtPosition(0)
    val newInternalPositionToValues:Array[(Int,Int)] = Array.fill[(Int,Int)](this.size)(null)
    var newValueToInternalPositions = RedBlackTreeMap.empty[RedBlackTreeMap[Int]]
    while (explorer match {
      case None => false
      case Some(position) =>
        newInternalPositionToValues(position.position) = (position.position,position.value)
        newValueToInternalPositions = internalInsertToValueToInternalPositions(position.value, position.position, newValueToInternalPositions)
        explorer = position.next
        true
    }) {}

    new ConcreteIntSequence(RedBlackTreeMap.makeFromSorted(newInternalPositionToValues),
      newValueToInternalPositions,
      PiecewiseLinearBijectionNaive.identity,
      newInternalPositionToValues.length, targetUniqueID)
  }

  override def commitPendingMoves : IntSequence = this

  override def unorderedContentNoDuplicate : List[Int] = valueToInternalPositions.keys
}

class IntSequenceIterator(var crawler:Option[IntSequenceExplorer]) extends Iterator[Int] {

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
  val value:Int
  def position:Int
  def next:Option[IntSequenceExplorer]
  def prev:Option[IntSequenceExplorer]
}


class ConcreteIntSequenceExplorer(sequence:ConcreteIntSequence,
                                  override val position:Int,
                                  positionInRB:RBTMPosition[Int],
                                  currentPivotPosition:Option[RBTMPosition[Pivot]],
                                  pivotAbovePosition:Option[RBTMPosition[Pivot]])(
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

abstract class StackedUpdateIntSequence extends IntSequence(){
  override def delete(pos : Int, fast:Boolean,autoRework:Boolean) : IntSequence = {
    require(pos >= 0, "pos=" + pos + " for delete on UniqueIntSequence should be >= 0")
    require(pos < size, "cannot delete past end of sequence in UniqueIntSequence")
    new RemovedIntSequence(this,pos)
  }

  override def moveAfter(startPositionIncluded : Int, endPositionIncluded : Int, moveAfterPosition : Int, flip : Boolean, fast:Boolean,autoRework:Boolean) : IntSequence = {
    require(startPositionIncluded >= 0 && startPositionIncluded < size , "startPositionIncluded=" + startPositionIncluded + " should be in [0,size" + size + "[ in UniqueIntSequence.moveAfter")
    require(endPositionIncluded >= 0 && endPositionIncluded < size , "endPositionIncluded=" + endPositionIncluded +" should be in [0,size"+size+"[ in UniqueIntSequence.moveAfter")
    require(moveAfterPosition >= -1 && moveAfterPosition < size , "moveAfterPosition=" + moveAfterPosition + " should be in [-1,size="+size+"[ in UniqueIntSequence.moveAfter")

    new MovedIntSequence(this,startPositionIncluded,endPositionIncluded,moveAfterPosition,flip)
  }

  override def insertAtPosition(value : Int, pos : Int, fast:Boolean,autoRework:Boolean) : IntSequence = {
    require(pos >= 0 && pos <= size , "pos=" + pos + " should be in [0,size="+size+"] in UniqueIntSequence.insertAt")
    new InsertedIntSequence(this,value:Int,pos:Int)
  }


  override def regularizeToMaxPivot(maxPivot: Int, targetUniqueID: Int = this.uniqueID) : ConcreteIntSequence =
    commitPendingMoves.regularizeToMaxPivot(maxPivot : Int, targetUniqueID : Int)

  override def regularize(targetUniqueID:Int = this.uniqueID) : ConcreteIntSequence = commitPendingMoves.regularize(targetUniqueID)
}

object MovedIntSequence{
  def bijectionForMove(startPositionIncluded:Int,
                      endPositionIncluded:Int,
                      moveAfterPosition:Int,
                      flip:Boolean):PiecewiseLinearBijectionNaive = {
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
  }
}

class MovedIntSequence(val seq:IntSequence,
                       startPositionIncluded:Int,
                       endPositionIncluded:Int,
                       moveAfterPosition:Int,
                       flip:Boolean)
  extends StackedUpdateIntSequence{

  override def unorderedContentNoDuplicate : List[Int] = seq.unorderedContentNoDuplicate

  override def descriptorString : String = seq.descriptorString + ".moved(startPos:" + startPositionIncluded + " endPos:" + endPositionIncluded + " targetPos:" + moveAfterPosition + " flip:" + flip + ")"

  val localBijection = MovedIntSequence.bijectionForMove(startPositionIncluded, endPositionIncluded, moveAfterPosition, flip)

  override val size : Int = seq.size

  override def nbOccurrence(value : Int) : Int = seq.nbOccurrence(value)

  override def commitPendingMoves:IntSequence = seq.commitPendingMoves.moveAfter(startPositionIncluded,endPositionIncluded,moveAfterPosition,flip,fast=false,autoRework = false)

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

  override def positionsOfValue(value : Int) : SortedSet[Int] = {
    seq.positionsOfValue(value).map(localBijection.backward(_))
  }

  override def contains(value : Int) : Boolean = seq.contains(value)

  override def isEmpty : Boolean = seq.isEmpty

  override def valueAtPosition(position : Int) : Option[Int] = {
    seq.valueAtPosition(localBijection.forward(position))
  }
}

class MovedIntSequenceExplorer(sequence:MovedIntSequence,
                               override val position:Int,
                               positionInBasicSequence:IntSequenceExplorer,
                               currentPivotPosition:Option[RBTMPosition[Pivot]],
                               pivotAbovePosition:Option[RBTMPosition[Pivot]])(
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

class InsertedIntSequence(seq:IntSequence,
                          val value:Int,
                          val pos:Int)
  extends StackedUpdateIntSequence {
  override val size : Int = seq.size + 1

  override def nbOccurrence(value : Int) : Int = if(value == this.value) seq.nbOccurrence(value) + 1 else seq.nbOccurrence(value)

  override def descriptorString : String = seq.descriptorString + ".inserted(val:" + value + " pos:" + pos + ")"

  override def unorderedContentNoDuplicate : List[Int] = if(seq.nbOccurrence(value) == 0) value :: seq.unorderedContentNoDuplicate else seq.unorderedContentNoDuplicate

  override def positionsOfValue(value : Int) : SortedSet[Int] = {
    val translatedPos:SortedSet[Int] = seq.positionsOfValue(value).map(oldPOsition => oldPos2NewPos(oldPOsition))
    if(value == this.value) translatedPos.+(pos)
    else translatedPos
  }

  private def oldPos2NewPos(oldPOs:Int):Int = {
    if(oldPOs < pos) oldPOs else oldPOs +1
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

  override def commitPendingMoves : IntSequence = seq.commitPendingMoves.insertAtPosition(value, pos, fast = false, autoRework = false)

  override def isEmpty : Boolean = false

  override def valueAtPosition(position : Int) : Option[Int] = {
    if (position == pos) Some(value)
    else if (position < pos) seq.valueAtPosition(position)
    else seq.valueAtPosition(position - 1)
  }
}

class InsertedIntSequenceExplorer(seq:InsertedIntSequence,
                                  val position:Int,
                                  explorerInOriginalSeq:Option[IntSequenceExplorer],
                                  atInsertedValue:Boolean,
                                  originalExplorerIsAbove:Boolean)
  extends IntSequenceExplorer {
  override val value : Int = if(atInsertedValue) seq.value else explorerInOriginalSeq.head.value

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

class RemovedIntSequence(seq:IntSequence,
                         val position:Int)
  extends StackedUpdateIntSequence{

  val removedValue = seq.valueAtPosition(position).head

  override def descriptorString : String = seq.descriptorString + ".removed(pos:" + position + " val:" + removedValue + ")"
  
  override def nbOccurrence(value : Int) : Int = if(value == this.removedValue) seq.nbOccurrence(value) - 1 else seq.nbOccurrence(value)

  override def unorderedContentNoDuplicate : List[Int] =
    if(nbOccurrence(removedValue) != 0) seq.unorderedContentNoDuplicate
    else seq.unorderedContentNoDuplicate.filter(_ != removedValue)

  override val size : Int = seq.size - 1

  override def explorerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    seq.explorerAtPosition(if (position < this.position) position else position + 1) match {
      case None => None
      case Some(e) => Some(new DeletedIntSequenceExplorer(this, position, e))
    }
  }

  override def positionsOfValue(value : Int) : SortedSet[Int] = {
    val oldPosSet = seq.positionsOfValue(value).-(position)
    oldPosSet.map(oldPos2NewPos)
  }

  def oldPos2NewPos(oldPos:Int) = {
    if (oldPos < this.position) oldPos else oldPos - 1
  }

  override def contains(value : Int) : Boolean = {
    val positions = seq.positionsOfValue(value)
    positions.size>1
  }

  override def commitPendingMoves : IntSequence = seq.commitPendingMoves.delete(this.position,fast=false,autoRework=false)

  override def valueAtPosition(position : Int) : Option[Int] = {
    if(position >= this.position) seq.valueAtPosition(position+1)
    else seq.valueAtPosition(position)
  }
}

class DeletedIntSequenceExplorer(seq:RemovedIntSequence,
                                 val position:Int,
                                 explorerInOriginalSeq:IntSequenceExplorer)
  extends IntSequenceExplorer{
  override val value : Int = explorerInOriginalSeq.value

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

