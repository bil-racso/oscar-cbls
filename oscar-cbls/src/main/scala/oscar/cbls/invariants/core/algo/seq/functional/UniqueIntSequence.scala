package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.fun.{PiecewiseLinearBijectionNaive, Pivot, LinearTransform}
import oscar.cbls.invariants.core.algo.rb.{RBPosition, RedBlackTree}

object UniqueIntSequence{
  def apply(maxPivot:Int = 10, maxSize:Int = 1000):UniqueIntSequence = new UniqueIntSequence(
    RedBlackTree.empty[Int],
    RedBlackTree.empty[Int],
    PiecewiseLinearBijectionNaive.identity,
    0,
    maxPivot,
    maxSize
  )
}

class UniqueIntSequence(private[seq] val internalPositionToValue:RedBlackTree[Int],
                        private[seq] val valueToInternalPosition:RedBlackTree[Int],
                        private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                        private[seq] val startFreeRangeForInternalPosition:Int,
                        maxPivot:Int = 10, maxSize:Int = 1000) {


  def check {
    require(internalPositionToValue.content.sortBy(_._1) equals valueToInternalPosition.content.map({case (a,b) => (b,a)}).sortBy(_._1))
  }

  private def swapInternalPositions(internalPosition1:Int,
                                    internalPosition2:Int,
                                    internalPositionToValue:RedBlackTree[Int],
                                    valueToInternalPosition:RedBlackTree[Int]): (RedBlackTree[Int],RedBlackTree[Int]) = {

    val value1 = internalPositionToValue.get(internalPosition1).head
    val value2 = internalPositionToValue.get(internalPosition2).head

    (internalPositionToValue.insert(internalPosition1,value2).insert(internalPosition2,value2),
      valueToInternalPosition.insert(value1,internalPosition2).insert(value2, internalPosition1))
  }

  override def toString : String = {
    "(size:" + size + ")[" + this.iterator.toList.mkString(",") + "] (\n" +
      "internalPositionToValue:" + internalPositionToValue.content +
      "\nvalueToInternalPosition:" + valueToInternalPosition.content +
      "\nexternalToInternalPosition:" + externalToInternalPosition + ")"
  }

  def size : Int = valueToInternalPosition.size

  def iterator : Iterator[Int] = new UniqueIntSequenceIterator(this)

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

  def isValueIncluded(value:Int):Boolean = valueToInternalPosition.contains(value)

  def crawlerAtPosition(position : Int) : Option[IntSequenceExplorer] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.forward.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition,internalPosition) = currentPivotPosition match {
        case None => (None,position)
        case Some(p) => (p.next,p.value.f(position))
      }

      Some(new IntSequenceExplorer(this,
        position,
        internalPositionToValue.positionOf(internalPosition).head,
        currentPivotPosition,
        pivotAbovePosition)()
      )
    }
  }

  def crawlerAtValue(value: Int) : Option[IntSequenceExplorer] = {
    positionOfValue(value) match{
      case None => None
      case Some(position) => crawlerAtPosition(position)
    }
  }

  def insertAtPosition(value:Int, pos:Int):UniqueIntSequence = {
    println("insertAtPosition(value:" + value + " pos:" + pos + ")")
    require(pos<=size,"inserting past the end of the sequence (size:" + size + " pos:" + pos + ")")
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
    }else{
      //inserting somewhere within the sequence, need to shift upper part
      externalToInternalPosition.updateBefore(
        (pos+1,size,LinearTransform(-1,false)),
        (pos,pos,LinearTransform(oldExternalPosRelatedToFreeInternalPos-pos,false)))
    }

    new UniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1,
      maxPivot,maxSize)
  }

  def delete(pos:Int):UniqueIntSequence = {
    println("delete(pos:" + pos + ")")
    require(pos<size,"deleting past the end of the sequence (size:" + size + " pos:" + pos + ")")
    require(pos>=0,"deleting at negative pos:" + pos)

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
        LinearTransform(pos-largestInternalPosition,false)),
      (pos,pos,LinearTransform(largestInternalPosition - pos,false)))


    new UniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition -1,
      maxPivot,maxSize)
  }

  def moveAfter(startPosition:Int, endPosition:Int, moveAfterPosition:Int, flip:Boolean):UniqueIntSequence = null
  def regularize:UniqueIntSequence = null
}

class UniqueIntSequenceIterator(s:UniqueIntSequence) extends Iterator[Int] {
  var crawler = s.crawlerAtPosition(0)

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

class IntSequenceExplorer(sequence:UniqueIntSequence,
                          val position:Int,
                          positionInRB:RBPosition[Int],
                          currentPivotPosition:Option[RBPosition[Pivot]],
                          pivotAbovePosition:Option[RBPosition[Pivot]])(
                           limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                             case None => sequence.externalToInternalPosition.forward.firstPivot.head._1
                             case Some(p) => p.value.fromValue-1},
                           limitBelowForCurrentPivot:Int = currentPivotPosition match{
                             case None => Int.MinValue
                             case Some(p) => p.value.fromValue},
                           slopeIsPositive:Boolean = currentPivotPosition match{
                             case None => true
                             case Some(p) => !p.value.f.minus}
                           ) {

  override def toString : String = "IntSequenceExplorer(position:" + position + " value:" + value + " positionInRB:" + positionInRB + ")"

  val value : Int = positionInRB.value

  def next : Option[IntSequenceExplorer] = {
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head

      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInRBOpt = sequence.internalPositionToValue.positionOf(pivotAbovePosition.head.value.f(newPosition))
      newPositionInRBOpt match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new IntSequenceExplorer(sequence,
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
          Some(new IntSequenceExplorer(sequence,
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

  def prev : Option[IntSequenceExplorer] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot
      val newPosition = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition.head.value.f(newPosition)
      val newCurrentPositionInRB = sequence.internalPositionToValue.positionOf(newInternalPosition).head
      Some(new IntSequenceExplorer(sequence,
        newPosition,
        newCurrentPositionInRB,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      Some(new IntSequenceExplorer(sequence,
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

