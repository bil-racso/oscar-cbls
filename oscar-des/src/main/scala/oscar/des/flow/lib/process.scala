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

package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core._

import scala.collection.mutable.ListBuffer

trait HelperForProcess{
  implicit def floatToConstantFloatFunction(f: Float): (() => Float) = (() => f)
  implicit def intToConstantFloatFunction(f: Int): (() => Float) = (() => f)
  implicit def floatToConstantIntFunction(f: Float): (() => Int) = (() => f.toInt)
  implicit def intToConstantIntFunction(f: Int): (() => Int) = (() => f)
}

/**
 * This represents a batch process (see [[SingleBatchProcess]]) with multiple batch running in parallell.
 * @param numberOfBatches the number of batches running in parallell.
 * @param m the simulation model
 * @param batchDuration the duration of a batch starting from all inputs being inputted, and ending with the beginning of the outputting
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing, bath are named "name chain i" where i is the identifier of the batch process
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 * */
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
 * @param m the simulation model
 * @param batchDuration the duration of a batch starting from all inputs being inputted, and ending with the beginning of the outputting
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 * */
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
 *
 * @param m the simulation model
 * @param processDuration the duration between inputting a batch and outûtting the batch
 * @param minimalSeparationBetweenBatches the minimal separation between two consecutive batches
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
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
 * this will put some delay between the incoming and outgoing of goods.
 * Suppose a conveyor belt where you can stack things,
 * and that is not stopped by an overflow at the destination (things accumulate at the output in an ugly way)
 * also, thus implementation is much less efficient than the one of [[ConveyerBeltProcess]]
 * @param m
 * @param delay
 */
class Delay(m:Model, delay:Float, destination:Puteable) extends RichPuteable{

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
 *This policy fills in a stock when it is below some threshold by placing an order to a supplier.
 * The storage will be refurbished when the supplier actually delivers the order
 *
 * @param s the storage that is refurbished through this policy
 * @param threshold the order is placed as soon as the stock gets below this threshold
 * @param orderQuantity given the actual level of the stock, how much do we order?
 * @param supplier the supplier at which the order will be placed
 * @param verbose true to print order placement on the console
 * @param name a name used for pretty printing
 * @author renaud.delandtsheer@cetic.be
 * */
class OrderOnStockTreshold(s:Storage, threshold:Int, orderQuantity:Int=>Int, supplier:PartSupplier, verbose:Boolean = true, name:String)
  extends NotificationTarget{
  s.registernotificationTarget(this)

  var placedOrders = 0

  var lastNotifiedlevel:Int = s.content
  override def notifyStockLevel(level: Int): Unit = {
    if(level <= threshold && lastNotifiedlevel > threshold){
      performOrder()
    }
    lastNotifiedlevel = level
  }

  protected def performOrder(): Unit ={
    val orderedQuantity = orderQuantity(s.content)
    if (verbose) println("threshold (" + threshold + ") reached on " + s.name + " (now:" + s.content + "), ordered " + orderedQuantity + " parts to " + supplier.name)
    supplier.order(orderedQuantity, s)
    placedOrders += 1
  }

  override def toString: String = name + " placed " + placedOrders + " orders"
}

/**
 *This policy fills in a stock when it is below some threshold by placing an order to a supplier.
 * The storage will be refurbished when the supplier actually delivers the order
 * The storage is actually checked every period for its level.
 * @param s the storage that is refurbished through this policy
 * @param m the model of the simulation
 * @param threshold the order is placed as soon as the stock gets below this threshold
 * @param period the period of time where the stock is checked
 * @param orderQuantity given the actual level of the stock, how much do we order?
 * @param supplier the supplier at which the order will be placed
 * @param verbose true to print order placement on the console
 * @param name a name used for pretty printing
 * @author renaud.delandtsheer@cetic.be
 * */
class OrderOnStockThresholdWithTick(s:Storage, m:Model, threshold:Int, period:Float, orderQuantity:Int=>Int, supplier:PartSupplier, verbose:Boolean = true, name:String)
  extends OrderOnStockTreshold(s, threshold, orderQuantity, supplier, verbose, name) {

  override def performOrder(){
    //the order is placed at a round up period after now
    m.wait(period - (m.clock() % period)) {super.performOrder()}
  }
}

/**
 * represents a supplier. the main operation is order
 * @param m the model of the simulation
 * @param supplierDelay the delay of the supplier (random function)
 * @param deliveredPercentage the delivered percentage, when an order is placed
 * @param name the name of the supplier, for pretty printing purpose
 * @param verbose true to print order deliveries on the console
 * @author renaud.delandtsheer@cetic.be
 * */
class PartSupplier(m:Model, supplierDelay:()=>Int, deliveredPercentage:() => Int, val name:String, verbose:Boolean = true){
  var placedOrders = 0
  var totalOrderedParts = 0
  var deliveredOrders = 0
  var totalDeliveredParts = 0

  def order(orderQuantity:Int, to:Storage): Unit ={
    totalOrderedParts += orderQuantity
    placedOrders += 1
    val willBeDelivered = (deliveredPercentage() * orderQuantity) / 100
    m.wait(supplierDelay()){
      if(verbose) println(name + ": delivered " + willBeDelivered + " parts to stock " + to.name +
        (if (willBeDelivered != orderQuantity) " (ordered: " + orderQuantity + ")" else ""))
      to.put(willBeDelivered){totalDeliveredParts += willBeDelivered; deliveredOrders +=1}
    }
  }

  override def toString: String = name + " receivedOrders:" + placedOrders + " totalOrderedParts:" + totalOrderedParts + " deliveredOrders:" + deliveredOrders + " totalDeliveredParts " + totalDeliveredParts
}

/**
 * represents a storage point, or a stock as you name it
 * This storage overflows when it is too much filled in
 * @param size the maximal content of the stock. attempting to put more items will lead t oan overflow, with loss of stock content
 * @param initialContent the initial content of the stock
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @author renaud.delandtsheer@cetic.be
 * */
case class OverflowStorage(override val size:Int,
                           initialContent:Int,
                           override val name:String,
                           override val verbose:Boolean=true)
  extends Storage(size,initialContent, name, verbose){
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

/**
 * represents a storage point, or a stock as you name it
 * @param size the maximal content of the stock. attempting to put more items will block the putting operations
 * @param initialContent the initial content of the stock
 * @param name the name of the stock
 * @param verbose true to print when stock is empty or overfull
 * @author renaud.delandtsheer@cetic.be
 * */
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
