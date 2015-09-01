package oscar.des.flow.lib

import oscar.des.flow.core.ItemClassHelper._
import oscar.des.flow.core._

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/**
 * represents a storage point, or a stock as you name it
 * @param maxSize the maximal content of the stock. attempting to put more items will block the putting operations
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class Storage(val maxSize: Int,
                       val name:String,
                       val verbose:Boolean,
                       overflowOnInput:Boolean)
  extends RichPutable with RichFetchable {

  class BufferCompositeItem(var n:Int, val itemClass:ItemClass)
  implicit def coupleToComposite(c:(Int,ItemClass)):BufferCompositeItem = new BufferCompositeItem(c._1,c._2)

  def totalFetch:Int = pTotalFetch
  def totalPut:Int = pTotalPut

  var totalLosByOverflow = 0

  def contentSize:Int

  private var notificationTo: List[StockNotificationTarget] = List.empty

  override def toString: String = {
    name + " " + this.getClass.getSimpleName + ":: content:" + contentSize + " max:" + maxSize + " totalPut:" + totalPut + " totalFetch:" + totalFetch + (if(overflowOnInput) " totalOverflow:" + totalLosByOverflow else "")
  }

  protected def flow(): Boolean = {
    var somethingCouldBeDone = false
    var finished = false
    while (!finished) {
      finished = true
      if (processBlockedFetches()) {
        somethingCouldBeDone = true
        finished = false
      }
      if (processBlockedPuts()) {
        somethingCouldBeDone = true
        finished = false
      }
    }

    if (somethingCouldBeDone){
      //dirty but faster
      var tmp = notificationTo
      while(tmp.nonEmpty){
        tmp.head.notifyStockLevel(contentSize)
        tmp = tmp.tail
      }
    }

    if (overflowOnInput) {
      val lostByOverflow = flushBlockedPuts()
      totalLosByOverflow += lostByOverflow
      if (lostByOverflow != 0 && verbose) println(name + ": overflow, lost " + lostByOverflow)
      somethingCouldBeDone || (lostByOverflow != 0)
    }else {
      somethingCouldBeDone
    }
  }

  def registerNotificationTarget(t: StockNotificationTarget): Unit = {
    notificationTo = t :: notificationTo
  }

  def fetch(amount: Int)(block: ItemClass => Unit) {
    appendFetch(amount)(block)
    flow()
    if (isThereAnyWaitingFetch && verbose) println("Empty storage on " + name)
  }

  def put(amount: Int, i:ItemClass)(block: () => Unit): Unit = {
    appendPut(List((amount,i)))(block)
    flow()
    if (isThereAnyWaitingPut && verbose)
      println("Full storage on " + name)
  }
}

/**
 *this type of storage acts in a FIFO-way.
 * it does matter to know this if you distinguish between different items.
 * @param maxSize the maximal content of the stock. attempting to put more items will block the putting operations
 * @param initialContent the initial content of the stock
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @param overflowOnInput true if the stock overflows when there are excessing input, false to have it blocking the puts when it is full
 */
class FIFOStorage(maxSize:Int,
                  initialContent:List[(Int,ItemClass)],
                  name:String,
                  verbose:Boolean,
                  overflowOnInput:Boolean) extends Storage(maxSize,name,verbose,overflowOnInput){

  val content:ListBuffer[BufferCompositeItem] = ListBuffer.empty ++ initialContent.map(coupleToComposite)

  override def contentSize: Int = internalSize

  private[this] var internalSize = content.foldLeft(0)({case (acc,bufferItem) => acc + bufferItem.n})

  /**
   * @param l
   * @return what remains to be pt after this put, and what has been put
   */
  override protected def internalPut(l: List[(Int,ItemClass)], hasBeenPut:Int = 0): (List[(Int,ItemClass)], Int) = {
    l match {
      case h :: t =>
        val n = h._1
        if(n + internalSize <= maxSize) {
          //we can put the header composite fully
          if (content.nonEmpty && content.last.itemClass == h._2){
            //we can aggregate wit hthe previously put item because tehy have the same ItemClass :-)
            content.last.n += n
          }else {
            content.prepend(h)
          }
          internalSize += n
          internalPut(t, hasBeenPut + n) //tail

        }else if (internalSize < maxSize){
          //only partial put, and end of recursion
          val toPut = maxSize - internalSize
          val remain = n - toPut
          if (content.nonEmpty && content.last.itemClass == h._2){
            //we can aggregate wit hthe previously put item because they have the same ItemClass :-)
            content.last.n += toPut
          }else {
            content.prepend((toPut,h._2))
          }
          internalSize += toPut
          ((remain,h._2) :: t,hasBeenPut + toPut)
        }else {
          (l, hasBeenPut)
        }
      case Nil => (l, hasBeenPut)
    }
  }


  override protected def internalFetch(remainsToFetch: Int, hasBeenFetch:ItemClass = ItemClassHelper.zeroItemClass): (Int,ItemClass) = {
    if(remainsToFetch == 0){
      (0,hasBeenFetch)
    }else if (content.nonEmpty){
      val head = content.head
      if(head.n <= remainsToFetch){
        content.remove(0)
        internalSize -= head.n
        internalFetch(remainsToFetch-head.n, ItemClassHelper.union(head.itemClass,hasBeenFetch))
      }else{
        //we do not take everything, we have to put a part back
        head.n -= remainsToFetch
        internalSize -= remainsToFetch
        (0,ItemClassHelper.union(head.itemClass,hasBeenFetch))
      }
    }else{
      (remainsToFetch,hasBeenFetch)
    }
  }
}

 /**
 *this type of storage acts in a LIFO-way.
 * it does matter to know this if you distinguish between different items.
 * @param maxSize the maximal content of the stock. attempting to put more items will block the putting operations
 * @param initialContent the initial content of the stock
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @param overflowOnInput true if the stock overflows when there are excessing input, false to have it blocking the puts when it is full
 */
class LIFOStorage(maxSize:Int,
                  initialContent:List[(Int,ItemClass)] = List.empty,
                  name:String,
                  verbose:Boolean,
                  overflowOnInput:Boolean) extends Storage(maxSize,name,verbose,overflowOnInput){

  var content:List[BufferCompositeItem] = initialContent.map(coupleToComposite)

  override def contentSize: Int = internalSize
  private[this] var internalSize: Int = content.foldLeft(0)({case (acc,bufferItem) => acc + bufferItem.n})

  /**
   * @param l
   * @return what remains to be pt after this put, and what has been put
   */
  override protected def internalPut(l: List[(Int,ItemClass)], hasBeenPut:Int = 0): (List[(Int,ItemClass)], Int) = {
    l match {
      case h :: t =>
        val n = h._1
        if(n + internalSize <= maxSize) {
          //we can put the header composite fully (so far, we do not aggregate composite items)
          if(content.nonEmpty && content.head.itemClass == h._2){
            content.head.n += n
          }else{
            content = h :: content
          }
          internalSize += n
          internalPut(t, hasBeenPut + n) //tail
        }else if (internalSize < maxSize){
          //only partial put, and end of recursion
          val toPut = maxSize - internalSize
          val remain = n - toPut
          if(content.nonEmpty && content.head.itemClass == h._2){
            content.head.n += toPut
          }else{
            content = (toPut,h._2) :: content
          }
          internalSize += toPut
          ((remain,h._2) :: t,hasBeenPut + toPut)
        }else {
          (l, hasBeenPut)
        }
      case Nil => (l, hasBeenPut)
    }
  }


  override protected def internalFetch(remainsToFetch: Int, hasBeenFetch:ItemClass = ItemClassHelper.zeroItemClass): (Int,ItemClass) = {
    if(remainsToFetch == 0){
      (0,hasBeenFetch)
    }else content match{
      case head :: t =>
        if(head.n <= remainsToFetch){
          //we take this composite fully
          content = t
          internalSize -= head.n
          internalFetch(remainsToFetch-head.n, ItemClassHelper.union(head.itemClass,hasBeenFetch))
        }else{
          //we do not take everything, we have to put a part back
          head.n -= remainsToFetch
          internalSize -= remainsToFetch
          (0,ItemClassHelper.union(head.itemClass,hasBeenFetch))
        }
      case Nil =>
        (remainsToFetch,hasBeenFetch)
    }
  }
}

/*
class NonAttributeStorage[Content<:StockContentType]
(maxSize:Int,
 initialContent:Int,
 fetchedClass:ItemClass,
 name:String,
 verbose:Boolean,
 overflowOnInput:Boolean) extends Storage[Content](maxSize,name,verbose,overflowOnInput){

  override var contentSize: Int = initialContent

  /**
   * @param l
   * @return what remains to be pt after this put, and what has been put
   */
  override protected def internalPut(l: List[ItemClass], hasBeenPut:Int = 0): (List[ItemClass], Int) = {
    l match {
      case h :: t =>
        if(content.size < maxSize) {
          content = h :: content
          internalPut(t, hasBeenPut + 1)
        }else {
          (l, hasBeenPut)
        }
      case null => (l, hasBeenPut)
    }
  }

  /**
   * @param amount
   * @return what remains to be fetched, what has been fetched
   */
  override protected def internalFetch(remainsToFetch: Int, hasBeenFetch:List[ItemClass] = List.empty): (Int,List[ItemClass]) = {
    if (remainsToFetch == 0) {
      (0, hasBeenFetch)
    } else content match {
      case h :: t =>
        content = t
        internalFetch(remainsToFetch - 1, h :: hasBeenFetch)
      case null =>
        (remainsToFetch, hasBeenFetch)
    }
  }
}
*/