package oscar.cbls.invariants.core.algo.seq

class IntSequence extends Iterable[SeqPosition]{
  var headPhantom = SymSeqPosition(null,null,-1)
  var tailPhantom = SymSeqPosition(null,null,-2)

  headPhantom.b = tailPhantom
  tailPhantom.a = headPhantom

  protected[this] var mSize = 0
  override def size:Int = mSize

  protected def createNewSymPos(value:Int):SymSeqPosition = SymSeqPosition(null,null,value)
  protected def freeSymPos(pos:SymSeqPosition){}

  override def head:SeqPosition = {
    if(isEmpty) throw new Error(".head on empty Int Sequence")
    val theNeighbor = headPhantom.b
    SeqPosition(theNeighbor, (theNeighbor.a == headPhantom))
  }

  override def last:SeqPosition= {
    if(isEmpty) throw new Error(".last on empty Int Sequence")
    val theNeighbor = tailPhantom.a
    SeqPosition(theNeighbor, (theNeighbor.b == tailPhantom))
  }

  override def isEmpty: Boolean = (mSize == 0)

  override def iterator: Iterator[SeqPosition] = new IntSequenceIterator(SeqPosition(headPhantom,true))
  def toIntList:List[Int] = this.map(x => x.pos.value).toList //TODO: reduce overhead

  def insertAfter(pos:SeqPosition,value:Int):SeqPosition = {
    val oldNext = pos.next
    val newPos = SeqPosition(createNewSymPos(value))
    pos.setNextAndRelink(newPos)
    newPos.setNextAndRelink(oldNext)
    mSize +=1
    newPos
  }

  def insertBefore(pos:SeqPosition,value:Int):SeqPosition = {
    val oldPrev = pos.prev
    val newPos = SeqPosition(createNewSymPos(value))
    oldPrev.setNextAndRelink(newPos)
    newPos.setNextAndRelink(pos)
    mSize += 1
    newPos
  }

  def delete(pos:SeqPosition){delete(pos.pos)}
  def delete(pos:SymSeqPosition){
    pos.a.replace(pos,pos.b)
    pos.b.replace(pos,pos.a)
    mSize -=1
    freeSymPos(pos)
  }

  /** supposed to be in the same intSequence
    *
    * @param start
    * @param end
    * @param moveAfterTarget
    * @param flip
    */
  def moveAfter(start:SeqPosition, end:SeqPosition, moveAfterTarget:SeqPosition, flip:Boolean){
    //perform the cut
    val oldPrev = start.prev
    val oldNext = end.next
    oldPrev.setNextAndRelink(oldNext)
    //perform the flip, if needed
    val (newStart,newEnd) = if(flip){
      end.flip
      start.flip
      ((end,start))
    }else ((start,end))

    //perform the insert
    val newNext = moveAfterTarget.next
    moveAfterTarget.setNextAndRelink(newStart)
    newEnd.setNextAndRelink(newNext)
  }

  def moveAfter(moved:SymSeqPosition, moveAfterTarget:SeqPosition){
    val a = moved.a
    val b = moved.b
    a.replace(moved,b)
    b.replace(moved,a)
    val moveAfterTargetNextPos = moveAfterTarget.next.pos

    moveAfterTarget.pos.replace(moveAfterTargetNextPos,moved)
    moveAfterTargetNextPos.replace(moveAfterTarget.pos,moved)
    moved.a = moveAfterTarget.pos
    moved.b = moveAfterTargetNextPos
  }

  def moveBetween(moved:SymSeqPosition, ba:SymSeqPosition, bb:SymSeqPosition){
    val a = moved.a
    val b = moved.b
    a.replace(moved,b)
    b.replace(moved,a)

    ba.replace(bb,moved)
    bb.replace(ba,moved)
    moved.a = ba
    moved.b = bb
  }
}

class UniqueIntSequence(val maxSize:Int) extends IntSequence{

  private val allElments:Array[SymSeqPosition] = Array.fill[SymSeqPosition](maxSize)(null)

  override def freeSymPos(pos:SymSeqPosition){
    assert(allElments(pos.value) != null)
    allElments(pos.value) = null
  }

  override def createNewSymPos(value:Int): SymSeqPosition ={
    require(allElments(value) == null,"UniqueIntSequence requires that each value appears at most once, duplicate use of " + value)
    val newSymPos = SymSeqPosition(null,null,value)
    allElments(value) = newSymPos
    newSymPos
  }

  def symPosAt(value:Int):SymSeqPosition = allElments(value)
}

class IntSequenceIterator(var currentPosition:SeqPosition) extends Iterator[SeqPosition]{
  override def hasNext: Boolean = currentPosition.hasNext

  override def next(): SeqPosition = {
    currentPosition = currentPosition.next
    currentPosition
  }
}

case class SymSeqPosition(var a:SymSeqPosition, var b:SymSeqPosition, val value:Int){
  def replace(oldPos:SymSeqPosition,newPos:SymSeqPosition){
    if(oldPos == a){
      a = newPos
    }else{
      b = newPos
    }
  }
}

case class SeqPosition(pos:SymSeqPosition, var headIsOnA:Boolean = true){
  implicit def toPos:SymSeqPosition = pos

  def flip {headIsOnA = !headIsOnA}
  /**does not perform the cut*/
  def setNextAndRelink(newNext:SeqPosition){
    if(headIsOnA) pos.b = newNext.pos else pos.a = newNext.pos
    if(newNext.headIsOnA) newNext.pos.a = pos else newNext.pos.b = pos
  }

  /**does not perform the cut*/
  def setPrevAndRelink(newPrev:SeqPosition){
    if(headIsOnA) pos.a = newPrev.pos else pos.b = newPrev.pos
    if(newPrev.headIsOnA) newPrev.pos.b = pos else newPrev.pos.a = pos
  }

  //null if no such position exist
  def next:SeqPosition = {
    val theNeighbor = if (headIsOnA) pos.b else pos.a
    if(theNeighbor.b == null) return null
    SeqPosition(theNeighbor, (theNeighbor.a == pos))
  }

  def hasNext:Boolean = {
    val theNeighbor = if (headIsOnA) pos.b else pos.a
    (theNeighbor.b != null)
  }

  //null if no such position exist
  def prev:SeqPosition = {
    val theNeighbor = if (headIsOnA) pos.a else pos.b
    if(theNeighbor.a == null) return null
    SeqPosition(theNeighbor, (theNeighbor.b == pos))
  }

  def hasPrev:Boolean = {
    val theNeighbor = if (headIsOnA) pos.a else pos.b
    (theNeighbor.a != null)
  }
}
