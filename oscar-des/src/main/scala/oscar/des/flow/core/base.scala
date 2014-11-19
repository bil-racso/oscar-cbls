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
trait Puteable{
  /**
   * put the amount of goods into the puteable.
   * This is potentially blocking
   * @param amount
   * @param block
   */
  def put(amount:Int)(block : => Unit)
}

/** represents a process fragment from which one can fetch a part
  * @author renaud.delandtsheer@cetic.be
  * */
trait Fetcheable{
  /**
   * fetch the amount of goods from the puteable.
   * This is potentially blocking
   * @param amount
   * @param block
   */
  def fetch(amount:Int)(block : => Unit)
}

/** This proposes a standard FIFO model behind the fetch operation,
  * in case the fetch operation is not possible
  * and must wait eg. for some refurbishment to proceed
 * @author renaud.delandtsheer@cetic.be
 */
trait RichFetcheable extends Fetcheable{
  private val waitingFetches:ListBuffer[(Int, () => Unit)] = ListBuffer.empty
  protected var totalFetch = 0

  def isThereAnyWaitingFetch:Boolean = waitingFetches.nonEmpty

  /**
   *
   * @param amount
   * @return what remains to be fetched, whas has been fetched
   */
  protected def internalFetch(amount:Int):(Int,Int)

  def appendFetch(amount:Int)(block : => Unit){
    waitingFetches.append((amount, () => block))
  }

  /**
   * @return true if something could be done, false otherwise.
   */
  protected def processBlockedFetches():Boolean = {
    var somethingDone = false
    var finished = false
    while(! finished) {
      finished = true
      if(waitingFetches.nonEmpty){
        val (toFetch, block) = waitingFetches.remove(0)
        val (remainingToFetch,fetched) = internalFetch(toFetch)
        totalFetch += fetched
        if (remainingToFetch == 0) {
          block()
          finished = false
          somethingDone = true
        } else {
          if (remainingToFetch != toFetch) somethingDone = true
          waitingFetches.prepend((remainingToFetch, block))
          finished = true
        }
      }else{
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
trait RichPuteable extends Puteable{

  protected val waitingPuts:ListBuffer[(Int, () => Unit)] = ListBuffer.empty
  protected var totalPut = 0

  def isThereAnyWaitingPut:Boolean = waitingPuts.nonEmpty

  /**
   * @param amount
   * @return what remains to be pt after this put, and what has been put
   */
  protected def internalPut(amount:Int):(Int,Int)

  /**
   * put the amount of goods into the puteable.
   * This is potentially blocking
   * @param amount
   * @param block
   */
  protected def appendPut(amount:Int)(block : => Unit): Unit ={
    waitingPuts.append((amount, () => block))
  }

  /**
   * @return true if something could be done, false otherwise.
   */
  protected def processBlockedPuts(): Boolean ={
    var somethingDone = false
    var finished = false
    while(! finished) {
      finished = true
      if(waitingPuts.nonEmpty) {
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
      }else{
        finished = true
      }
    }
    somethingDone
  }

  /** flushes the blocked puts
    * the blocked method is called for each flushed put
    * @return the number of unnits that were flushed, zero if nothing was flushed
    */
  protected def flushBlockedPuts(): Int = {
    var flushedUnits = 0
    while (waitingPuts.nonEmpty) {
      val (toPut, block) = waitingPuts.remove(0)
      block()
      flushedUnits += toPut
      totalPut += toPut
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
case class CounterGate(waitedNotification:Int, gate: () => Unit){
  private var remaining=waitedNotification
  def notifyOne(): Unit ={
    remaining -=1
    if(remaining == 0) gate()
  }
}

/** This trait proposes a standard method to perform an output
  * an output consists is outputting a set of parts to a set of Puteables
  * @author renaud.delandtsheer@cetic.be
  */
trait Outputter{
  def outputs:List[(Int,Puteable)]
  val outputCount = outputs.length
  protected def performOutput(block: => Unit){
    val gate = CounterGate(outputCount +1, () => block)
    for((amount,puteable) <- outputs){
      puteable.put(amount){gate.notifyOne()}
    }
    gate.notifyOne()
  }
}

/** This trait proposes a standard method to perform an input
  * an input consists in fetching a set of parts from a set of Fetcheables
  * @author renaud.delandtsheer@cetic.be
  */
trait Inputter {
  def inputs:List[(Int,Fetcheable)]
  val inputCount = inputs.length
  protected def performInput(block : => Unit): Unit = {
    val gate = CounterGate(inputCount +1, () => block)
    for((amount,fetcheable) <- inputs){
      fetcheable.fetch(amount){gate.notifyOne()}
    }
    gate.notifyOne()
  }
}

trait NotificationTarget{
  def notifyStockLevel(level:Int)
}
