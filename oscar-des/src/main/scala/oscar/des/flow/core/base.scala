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

package oscar.des.flow.core

import scala.collection.mutable.ListBuffer



/** represents a process fragment where one can put a part
  * @author renaud.delandtsheer@cetic.be
  */
trait Putable {
  /**
   * put the amount of goods into the putable.
   * This is potentially blocking
   * @param amount: the items to be put in the puteable
   * @param block
   */
  def put(amount:Int,i:ItemClass)(block : () => Unit)
}

/** represents a process fragment from which one can fetch a part
  * @author renaud.delandtsheer@cetic.be
  * */
trait Fetchable {
  /**
   * fetch the amount of goods from the putable.
   * This is potentially blocking
   * @param amount
   * @param block the block to execute once the items are actually fetched. these are bigen to the block method
   */
  def fetch(amount:Int)(block : ItemClass => Unit)
}

/** This proposes a standard FIFO model behind the fetch operation,
  * in case the fetch operation is not possible
  * and must wait eg. for some refurbishment to proceed
 * @author renaud.delandtsheer@cetic.be
 */
trait RichFetchable extends Fetchable {
  private val waitingFetches:ListBuffer[(Int, List[ItemClass], List[ItemClass] => Unit)] = ListBuffer.empty
  protected var totalFetch = 0

  def isThereAnyWaitingFetch:Boolean = waitingFetches.nonEmpty

  /**
   * @param amount
   * @return what remains to be fetched, whas has been fetched
   */
  protected def internalFetch(amount:Int,hasBeenFetch:List[ItemClass] = List.empty):(Int,List[ItemClass])

  def appendFetch(amount:Int)(block : List[ItemClass] => Unit) {
    waitingFetches.append((amount, List.empty, block))
  }

  /**
   * @return true if something could be done, false otherwise.
   */
  protected def processBlockedFetches():Boolean = {
    var somethingDone = false
    var finished = false
    while (! finished) {
      finished = true
      if (waitingFetches.nonEmpty) {
        val (toFetch, alreadyFetched, block) = waitingFetches.remove(0)
        val (remainingToFetch,fetched) = internalFetch(toFetch)
        totalFetch += fetched.length
        val allFetchedForThisFetch = fetched ::: alreadyFetched
        if (remainingToFetch == 0) {
          block(allFetchedForThisFetch)
          finished = false
          somethingDone = true
        } else {
          if (remainingToFetch != toFetch) somethingDone = true
          waitingFetches.prepend((remainingToFetch, allFetchedForThisFetch, block))
          finished = true
        }
      } else {
        finished = true
      }
    }
    somethingDone
  }
}

/** This proposes a standard FIFO model behind the put operation,
  * in case the put operation is not possible
  * and must wait eg. for some space to be freed
  * @author renaud.delandtsheer@cetic.be
  */
trait RichPutable extends Putable {

  protected val waitingPuts:ListBuffer[(List[ItemClass], () => Unit)] = ListBuffer.empty
  protected var totalPut = 0

  def isThereAnyWaitingPut:Boolean = waitingPuts.nonEmpty

  /**
   * @param l
   * @return what remains to be pt after this put, and what has been put
   */
  protected def internalPut(l:List[ItemClass], hasBeenPut:Int = 0):(List[ItemClass],Int)

  /**
   * put the amount of goods into the putable.
   * This is potentially blocking
   * @param l the items to be put
   * @param block
   */
  protected def appendPut(l:List[ItemClass])(block : () => Unit): Unit = {
    waitingPuts.append((l, block))
  }

  /**
   * @return true if something could be done, false otherwise.
   */
  protected def processBlockedPuts(): Boolean = {
    var somethingDone = false
    var finished = false
    while (! finished) {
      finished = true
      if (waitingPuts.nonEmpty) {
        val (toPut, block) = waitingPuts.remove(0)
        val (remainingToPut,put) = internalPut(toPut)
        totalPut += put
        if (remainingToPut == 0) {
          block()
          finished = false
          somethingDone = true
        } else {
          if (remainingToPut != toPut) somethingDone = true
          waitingPuts.prepend((remainingToPut, block))
          finished = true
        }
      } else {
        finished = true
      }
    }
    somethingDone
  }

  /** flushes the blocked puts
    * the blocked method is called for each flushed put
    * @return the number of units that were flushed, zero if nothing was flushed
    */
  protected def flushBlockedPuts(): Int = {
    var flushedUnits = 0
    while (waitingPuts.nonEmpty) {
      val (toPut, block) = waitingPuts.remove(0)
      block()
      val nbToPut = toPut.length
      flushedUnits += nbToPut
      totalPut += nbToPut
    }
    flushedUnits
  }
}

/** This class counts a set of event,and calls a callback method
  * once a defined number of such events have happened.
  * @author renaud.delandtsheer@cetic.be
 *
 * @param waitedNotification the number of waited notifications
 * @param gate the method to call once the method notifyOne has been called waitedNotification times
 */
case class CounterGate(waitedNotification:Int, gate: () => Unit, var itemClass:ItemClass = null) {
  private var remaining = waitedNotification
  def notifyOne(mItemClass:ItemClass = null): Unit = {
    if(mItemClass != null){
      if (itemClass == null) itemClass = mItemClass
      else itemClass = itemClass union mItemClass
    }
    remaining -=1
    if(remaining == 0) gate()
  }
}

/** This trait proposes a standard method to perform an output
  * an output consists is outputting a set of parts to a set of Putables
  * @author renaud.delandtsheer@cetic.be
  */
class Outputter(outputs:Iterable[(() => Int, Putable)]) {
  val outputCount = outputs.size
  def performOutput(i:ItemClass, block: () => Unit){
    val gate = CounterGate(outputCount +1, block)
    for((nr,putable) <- outputs){
      putable.put(nr(),i)(() => gate.notifyOne())
    }
    gate.notifyOne()
  }
}

/** This trait proposes a standard method to perform an input
  * an input consists in fetching a set of parts from a set of Fetchables
  * @author renaud.delandtsheer@cetic.be
  */
class Inputter(inputs:Iterable[(() => Int, Fetchable)]) {
  val inputCount = inputs.size
  def addPreliminaryInput(preliminaryInput:Fetchable){
    require(preliminaryInput == null)
    this.preliminaryInput = preliminaryInput
  }
  var preliminaryInput:Fetchable = null

  def performInput(block : ItemClass => Unit): Unit = {

    def doPerformInput() {
      val gate = CounterGate(inputCount + 1, finished)
      def finished(): Unit = {
        block(gate.itemClass)
      }
      var i = 0;
      for ((amount, fetchable) <- inputs) {
        fetchable.fetch(amount())(gate.notifyOne)
        i += 1
      }
      gate.notifyOne()
    }

    if (preliminaryInput == null){
      doPerformInput()
    }else{
      preliminaryInput.fetch(1)(_ => doPerformInput())
    }
  }
}

trait StockNotificationTarget{
  def notifyStockLevel(level:Int)
}
