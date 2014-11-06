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

package oscar.des.flow

import oscar.des.engine.Model

import scala.collection.mutable.ListBuffer

case class BatchProcess(m:Model, numberOfBatches:Int, batchDuration:() => Float, inputs:List[(Int,Fetcheable)], outputs:List[(Int,Puteable)], name:String, verbose:Boolean = true){

  val childProcesses:Iterable[SingleBatchProcess] = (1 to numberOfBatches) map((batchNumber:Int) => SingleBatchProcess(m, batchDuration, inputs, outputs, name + " chain " + batchNumber, verbose))

  override def toString: String = {
    name + ":" + childProcesses.foldLeft(0)(_ + _.performedBatches) + " totalWaitDuration:" + childProcesses.foldLeft(0.0)(_ + _.totalWaitDuration)
  }
}

/**
 * a process inputs some inputs, and produces its outputs at a given rate.
 * notice that inputs and outputs are performed in parallell (thus might cause some deadlocks)
 *
 * @param m
 */
case class SingleBatchProcess(m:Model,
                              batchDuration:() => Float,
                              val inputs:List[(Int,Fetcheable)],
                              val outputs:List[(Int,Puteable)],
                              name:String,
                              verbose:Boolean = true) extends Inputter with Outputter{

  var performedBatches = 0

  var totalWaitDuration:Double = 0
  var startWaitTime:Double = 0

  startBatches()

  def startBatches(){
    if (verbose) println(name + ": start inputting")
    startWaitTime = m.clock()
    performInput {
      if (verbose) println(name + ": start new batch")
      totalWaitDuration += (m.clock() - startWaitTime)
      m.wait(batchDuration()){
        if (verbose) println(name + ": finished batch")
        startWaitTime = m.clock()
        performedBatches +=1
        performOutput {
          if (verbose) println(name + ": finished outputting")
          totalWaitDuration += (m.clock() - startWaitTime)
          startBatches()
        }
      }
    }
  }

  override def toString: String = {
    name + ":" + performedBatches + " totalWaitDuration:" + totalWaitDuration
  }
}


/**
 * rolling Process means that if the output is blocked, no new batch is started
 * (imagine an industrial rolling band oven where croissants are cooked)
 * and if the input is blocked, the output still proceeds, (as if we were starting empty batches) there is no catch up for the waited time
 * batch only start when they ave their complete inputs.
 * if the output is blocked, the process stops, thus does not perform new inputs either (we cannot model that croissants will eventually burn in the oven)
 */
class ConveyerBeltProcess(m:Model,
                          processDuration:Float,
                          minimalSeparationBetweenBatches:Float,
                          val inputs:List[(Int,Fetcheable)],
                          val outputs:List[(Int,Puteable)],
                          name:String,
                          verbose:Boolean = true) extends Outputter with Inputter{

  //the belt contains the delay for the output since the previous element that was input. delay since input if the belt was empty
  val belt: ListBuffer[Double] = ListBuffer.empty

  var timeOfLastInput:Double = 0

  var blocked:Boolean = false
  var startBlockingTime:Double = 0
  var outputMustBeRestarted:Boolean = true
  var inputMustBeRestarted:Boolean = true

  var blockedTimeSinceLastInput:Double = 0

  var totalInputBatches = 0
  var totalOutputBatches = 0
  var totalBlockedTime:Double = 0.0
  restartInputtingIfNeeded()


  override def toString: String = {
    name + " content: " + belt.size + " totalInputBatches:" + totalInputBatches + " totalOutputBatches:" + totalOutputBatches + " totalBlockedTime:" + totalBlockedTime
  }

  def restartInputtingIfNeeded(): Unit ={
    if(inputMustBeRestarted) {
      if(verbose) println(name + " restarting belt")
      inputMustBeRestarted = false
      startPerformInput()
    }
  }

  //on s'assure juste qu'il y aie assez de place depuis le dernier qu'on a mis dedans
  def startPerformInput() {
    val timeForNextInput = timeOfLastInput + minimalSeparationBetweenBatches + blockedTimeSinceLastInput

    if(blocked){
      if(verbose) println(name + " belt blocked at output, next input blocked")
      inputMustBeRestarted = true
    }else if(belt.isEmpty || timeForNextInput >= m.clock()){
      //we can perform the input
      if(verbose) println(name + " start input")
      startBlockingTime = m.clock()
      performInput(finishedInputs)
    }else{
      //we cannot do the input; need to sleep a bit because belt was blocked from output
      val durationToNextInput = timeForNextInput - m.clock()
      m.wait(durationToNextInput){startPerformInput()}
    }
  }

  def finishedInputs(): Unit ={
    if(belt.isEmpty){
      belt.prepend(processDuration)
    }else {
      belt.prepend(m.clock - timeOfLastInput)
    }
    timeOfLastInput = m.clock
    blockedTimeSinceLastInput = 0
    restartOutputtingIfNeeded()
    if(verbose) println(name + " input performed")
    totalInputBatches += 1
    m.wait(minimalSeparationBetweenBatches){startPerformInput()}
  }

  //called at input time, after belt has been fed again
  def restartOutputtingIfNeeded(){
    if(outputMustBeRestarted) {
      require(belt.nonEmpty)
      outputMustBeRestarted = false
      m.wait(belt.last) {
        startPerformOutput()
      }
    }
  }

  def startPerformOutput() {
    blocked = true
    startBlockingTime = m.clock()
    if(verbose) println(name + " start outputting")
    performOutput(finishedOutputs)
  }

  private def finishedOutputs(): Unit = {
    blocked = false
    blockedTimeSinceLastInput = (m.clock() - startBlockingTime)
    totalBlockedTime += blockedTimeSinceLastInput
    belt.remove(belt.size-1)
    if(verbose) println(name + " output performed")
    totalOutputBatches += 1
    restartInputtingIfNeeded()
    //otherwise, the belt is empty and the outputting process will be restarted by the next performed input.
    if (belt.nonEmpty) {
      m.wait(belt.last) {
        startPerformOutput()
      }
    }else{
      outputMustBeRestarted = true
      if(verbose) println(name + " belt is empty")
    }
  }
}


/**
 * this will put some delay between the incoming and outgoing of goods. suppose a conveyor belt
 * @param m
 * @param delay
 */
class Delay(m:Model, delay:Float, inputSlotDuration:Int, inputsPerSlot:Int, destination:Puteable) extends RichPuteable{

  var blocked = false
  /**
   * put the amount of goods into the puteable.
   * This is potentially blocking if the output is blocked
   * @param amount
   * @param block
   */
  override def put(amount: Int)(block: => Unit){
    appendPut(amount)({block; m.wait(delay) {output(amount)}})
    if(! blocked) processBlockedPuts()
  }

  private def output(amount:Int): Unit ={
    blocked = true
    destination.put(amount){blocked = false; processBlockedPuts()}
  }

  /** there is an unlimited input rate, actually, it is solely blocked if output is blocked
    *
    * @param amount
    * @return what remains to be pt after this put
    */
  //TODO: use maxInputRate!!
  override protected def internalPut(amount: Int): (Int,Int) = (0,0)
}


/**
 * @param s
 * @param threshold
 * @param orderQuantity given the actual level of the stock, how much do we order?
 * @param supplier
 */
case class OrderOnStockTreshold(s:Storage, threshold:Int, orderQuantity:Int=>Int, supplier:PartSupplier, verbose:Boolean = true)
  extends NotificationTarget{
  s.registernotificationTarget(this)

  var lastNotifiedlevel:Int = s.content
  override def notifyStockLevel(level: Int): Unit = {
    if(level <= threshold && lastNotifiedlevel > threshold){
      performOrder()
    }
    lastNotifiedlevel = level
  }

  def performOrder(): Unit ={
    val orderedQuantity = orderQuantity(s.content)
    if (verbose) println("threshold (" + threshold + ") reached on " + s.name + ", ordered " + orderedQuantity + " parts to " + supplier.name)
    supplier.order(orderedQuantity, s)
  }
}

trait NotificationTarget{
  def notifyStockLevel(level:Int)
}

class PartSupplier(m:Model, supplierDelay:()=>Int, deliveredPercentage:() => Int, val name:String, verbose:Boolean = true){
  def order(orderQuantity:Int, to:Storage): Unit ={
    val willBeDelivered = (deliveredPercentage() * orderQuantity) / 100
    m.wait(supplierDelay()){
      if(verbose) println(name + ": delivered " + willBeDelivered + " parts to stock " + to.name +
        (if (willBeDelivered != orderQuantity) " (ordered: " + orderQuantity + ")" else ""))
      to.put(willBeDelivered){}
    }
  }
}

trait Overflow extends Storage{
  //TODO: overflow should happen only once per simulation step (or kind of once) since all events are supposed to happen at the same time
  var totalLosByOverflow = 0
  override def flow():Boolean = {
    val toReturn = super.flow()
    val lostByOverflow = flushBlockedPuts()
    totalLosByOverflow += lostByOverflow
    if (lostByOverflow !=0 && verbose) println(name + ": overflow, lost " + lostByOverflow)
    toReturn || (lostByOverflow !=0)
  }

  override def toString: String = super.toString + " totalOverflow:" + totalLosByOverflow
}

/** connection points have no storage, they are just notified by the process that stuffs them in
  * they must have an output specified
  */
class Storage(val size:Int, initialContent:Int, val name:String, val verbose:Boolean=true) extends RichPuteable with RichFetcheable {
  var content:Int = initialContent

  private var notificationTo:List[NotificationTarget] = List.empty

  override def toString: String = {
    name + " content:" + content + " (max:" + size + ") totalPut:" + totalPut + " totalFetch:" + totalFetch
  }

  protected def flow() :Boolean = {
    var somethingCouldBeDone = false
    var finished = false
    while (!finished) {
      finished = true
      if (processBlockedFetches()){
        somethingCouldBeDone = true
        finished = false
      }
      if (processBlockedPuts()){
        somethingCouldBeDone = true
        finished = false
      }
    }

    if(somethingCouldBeDone)
      for(target <- notificationTo) target.notifyStockLevel(content)

    somethingCouldBeDone
  }

  def registernotificationTarget(t:NotificationTarget): Unit ={
    notificationTo = t :: notificationTo
  }

  def fetch(amount:Int)(block : => Unit){
    appendFetch(amount)(block)
    flow()
    if(isThereAnyWaitingFetch && verbose) println("Empty storage on " + name)
  }

  def put(amount:Int)(block : => Unit): Unit ={
    appendPut(amount)(block)
    flow()
    if(isThereAnyWaitingPut && verbose) println("Full storage on " + name)
  }

  /**
   * @param amount
   * @return what remains to be pt after this put
   */
  override protected def internalPut(amount: Int): (Int,Int) = {
    val newContent = content + amount
      if (newContent > size) {
        content = size
        val remainsToPut = newContent - size
        (remainsToPut,amount - remainsToPut)
      }else{
        content = newContent
        (0,amount)
      }
  }

  /**
   * @param amount
   * @return what remains to be fetched, what has been fetched
   */
  override protected def internalFetch(amount: Int): (Int,Int) = {
    val newContent = content - amount
    if (newContent >= 0) {
      content = newContent
      (0,amount)
    }else{
      content = 0
      (- newContent, amount + newContent)
    }
  }
}
