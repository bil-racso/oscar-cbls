package oscar.des.flow

import scala.collection.mutable.ListBuffer

trait Puteable{
  /**
   * put the amount of goods into the puteable.
   * This is potentially blocking
   * @param amount
   * @param block
   */
  def put(amount:Int)(block : => Unit)
}

trait Fetcheable{
  /**
   * fetch the amount of goods from the puteable.
   * This is potentially blocking
   * @param amount
   * @param block
   */
  def fetch(amount:Int)(block : => Unit)
}


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

case class CounterGate(waitedNotification:Int, gate: () => Unit){
  private var remaining=waitedNotification
  def notifyOne(): Unit ={
    remaining -=1
    if(remaining == 0) gate()
  }
}

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