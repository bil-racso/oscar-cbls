package oscar.des.flow.lib

import oscar.des.flow.core._

import scala.collection.mutable.ListBuffer

class StockContentType
class Orders extends StockContentType
class Items extends StockContentType


/**
 * represents a storage point, or a stock as you name it
 * @param size the maximal content of the stock. attempting to put more items will block the putting operations
 * @param initialContent the initial content of the stock
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class Storage[Content<:StockContentType](val maxSize: Int,
                                                  val name:String,
                                                  val verbose:Boolean,
                                                  overflowOnInput:Boolean)
  extends RichPutable with RichFetchable {

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

    if (somethingCouldBeDone)
      for (target <- notificationTo) target.notifyStockLevel(contentSize)

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
    appendFetch(amount)(l => block(ItemClassHelper.unionAll(l)))
    flow()
    if (isThereAnyWaitingFetch && verbose) println("Empty storage on " + name)
  }

  def put(amount: Int, i:ItemClass)(block: () => Unit): Unit = {
    appendPut(List.fill(amount)(i))(block)
    flow()
    if (isThereAnyWaitingPut && verbose) println("Full storage on " + name)
  }
}

class FIFOStorage[Content<:StockContentType](maxSize:Int,
                                             initialContent:List[ItemClass],
                                             name:String,
                                             verbose:Boolean,
                                             overflowOnInput:Boolean) extends Storage[Content](maxSize,name,verbose,overflowOnInput){

  val content:ListBuffer[ItemClass] = ListBuffer.empty

  override def contentSize: Int = content.size

  /**
   * @param l
   * @return what remains to be pt after this put, and what has been put
   */
  override protected def internalPut(l: List[ItemClass], hasBeenPut:Int = 0): (List[ItemClass], Int) = {
    l match {
      case h :: t =>
        if(content.size < maxSize) {
          content.prepend(h)
          internalPut(t, hasBeenPut + 1)
        }else {
          (l, hasBeenPut)
        }
      case Nil => (l, hasBeenPut)
    }
  }

  /**
   * @param amount
   * @return what remains to be fetched, what has been fetched
   */
  override protected def internalFetch(remainsToFetch: Int, hasBeenFetch:List[ItemClass] = List.empty): (Int,List[ItemClass]) = {
    if(remainsToFetch == 0){
      (0,hasBeenFetch)
    }else if (content.nonEmpty){
      internalFetch(remainsToFetch-1, content.remove(0) :: hasBeenFetch)
    }else{
      (remainsToFetch,hasBeenFetch)
    }
  }
}

class LIFOStorage[Content<:StockContentType](maxSize:Int,
                                             initialContent:List[ItemClass] = List.empty,
                                             name:String,
                                             verbose:Boolean,
                                             overflowOnInput:Boolean) extends Storage[Content](maxSize,name,verbose,overflowOnInput){

  var content:List[ItemClass] = List.empty

  override def contentSize: Int = content.size

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
      case Nil => (l, hasBeenPut)
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
      case Nil =>
        (remainsToFetch, hasBeenFetch)
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