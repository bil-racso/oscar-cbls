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
import scala.language.implicitConversions
import oscar.des.flow.core.ItemClassHelper._

/** since many of the classes proposed by this lib support random variables, represented using functions to Floats or Ints,
  * and ou might not need this dimension in your model,
  * this trait provides implicit translation eg. from Float to function to Float
  */
trait HelperForProcess{
  implicit def floatToConstantFloatFunction(f: Float): (() => Float) = () => f
  implicit def intToConstantFloatFunction(f: Int): (() => Float) = () => f
  implicit def floatToConstantIntFunction(f: Float): (() => Int) = () => f.toInt
  implicit def intToConstantIntFunction(f: Int): (() => Int) = () => f
  implicit def constantFetchableToFunctionFetchable(l: List[(Int,Fetchable)]): List[(()=>Int,Fetchable)] = l.map(v => (()=>v._1,v._2))
  implicit def constantPutableToFunctionPutable(l: List[(Int,Putable)]): List[(()=>Int,Putable)] = l.map(v => (()=>v._1,v._2))

}

/**
 * a process inputs some inputs, and produces its outputs at a given rate.
 * notice that inputs and outputs are performed in parallel (thus might cause some deadlocks)
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
                              inputs:Array[(() => Int, Fetchable)],
                              outputs:Array[(()=>Int,Putable)],
                              transformFunction:ItemClass => ItemClass,
                              name:String,
                              verbose:Boolean = true) extends ActivableAtomicProcess(name,verbose){

  private val myOutput = new Outputter(outputs)
  override val myInput = new Inputter(inputs)

  var performedBatches = 0

  private var mTotalWaitDuration:Double = 0
  private var startWaitTime:Double = 0
  private var waiting = false

  def isWaiting = waiting

  override def isRunning: Boolean = !waiting
  override def batchCount: Int = performedBatches
  override def totalWaitDuration():Double = if (waiting) mTotalWaitDuration + m.clock() - startWaitTime else mTotalWaitDuration

  startBatches()

  private def startBatches(){
    if (verbose) println(name + ": start inputting")
    startWaitTime = m.clock()
    waiting = true
    myInput.performInput((i:ItemClass) => {
      if (verbose) println(name + ": start new batch")
      mTotalWaitDuration += (m.clock() - startWaitTime)
      waiting = false
      m.wait(batchDuration()){
        if (verbose) println(name + ": finished batch")
        startWaitTime = m.clock()
        waiting = true
        performedBatches +=1
        myOutput.performOutput(transformFunction(i), () => {
          if (verbose) println(name + ": finished outputting")
          mTotalWaitDuration += (m.clock() - startWaitTime)
          waiting = false
          startBatches()
        })
      }
    })
  }

  override def toString: String = {
    name + " " + this.getClass.getSimpleName + ":: performedBatches:" + performedBatches + " totalWaitDuration:" + totalWaitDuration + (if (waiting) " waiting" else " running")
  }
}

/**
 * This represents a batch process (see [[SingleBatchProcess]]) with multiple batch running in parallel.
 * @param numberOfBatches the number of batches running in parallel.
 * @param m the simulation model
 * @param batchDuration the duration of a batch starting from all inputs being inputted, and ending with the beginning of the outputting
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing, bath are named "name chain i" where i is the identifier of the batch process
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 * */
case class BatchProcess(m:Model,
                        numberOfBatches:Int,
                        batchDuration:() => Float,
                        inputs:Array[(() => Int, Fetchable)],
                        outputs:Array[(() => Int,Putable)],
                        name:String,
                        transformFunction:ItemClass => ItemClass,
                        verbose:Boolean = true) extends ActivableMultipleProcess(name,verbose){

  override val childProcesses:Iterable[SingleBatchProcess] =
    (1 to numberOfBatches) map((batchNumber:Int) => SingleBatchProcess(m:Model,
      batchDuration,
      inputs,
      outputs,
      transformFunction,
      name + " chain " + batchNumber,
      verbose))

  override def toString: String = {
    name + " " + this.getClass.getSimpleName + ":: lines:" + numberOfBatches +" performedBatches:" + childProcesses.foldLeft(0)(_ + _.performedBatches) +
      " totalWaitDuration:" + childProcesses.foldLeft(0.0)(_ + _.totalWaitDuration) +
      " waitingLines:" + childProcesses.foldLeft(0)((waitings:Int,p:SingleBatchProcess) => waitings + (if (p.isWaiting) 1 else 0))
  }
}

/**
 * A process inputs some inputs, and produces its outputs at a given rate.
 * notice that inputs and outputs are performed in parallel (thus might cause some deadlocks)
 * this process might fail. In this case, failure is assessed at the end of the batch duration,
 * and produces the failureOutputs
 *
 * @param m the simulation model
 * @param batchDuration the duration of a batch starting from all inputs being inputted, and ending with the beginning of the outputting
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 * */
case class SplittingSingleBatchProcess(m:Model,
                                 batchDuration:() => Float,
                                 inputs:Array[(() => Int, Fetchable)],
                                 outputs:Array[Array[(() => Int,Putable)]],
                                 name:String,
                                 transformFunction:ItemClass => (Int,ItemClass),
                                 verbose:Boolean = true) extends ActivableAtomicProcess(name,verbose){

  private val myOutputs = outputs.map(o => new Outputter(o))
  override val myInput = new Inputter(inputs)

  var performedBatches = 0
  private var failedBatches = 0

  private var mTotalWaitDuration: Double = 0
  private var startWaitTime: Double = 0
  private var waiting = false //waiting at an input or output

  def isWaiting = waiting


  override def isRunning: Boolean = !waiting

  override def batchCount: Int = performedBatches

  override def totalWaitDuration():Double = if (waiting) mTotalWaitDuration + m.clock() - startWaitTime else mTotalWaitDuration

  startBatches()

  private def startBatches(){
    if (verbose) println(name + ": start inputting")
    startWaitTime = m.clock()
    waiting = true
    myInput.performInput((i:ItemClass) => {
      if (verbose) println(name + ": start new batch")
      mTotalWaitDuration += (m.clock() - startWaitTime)
      waiting = false
      m.wait(batchDuration()){
        startWaitTime = m.clock()
        waiting = true
        performedBatches +=1
        val (outputPort,outputi) = transformFunction(i)
        if (verbose) println(name + ": finished batch, outputting to " + outputPort)
        myOutputs(outputPort).performOutput(outputi, () => {
          if (verbose) println(name + ": finished outputting")
          mTotalWaitDuration += (m.clock() - startWaitTime)
          waiting = false
          startBatches()
        })
      }
    })
  }

  override def toString: String = {
    name + " " + this.getClass.getSimpleName + ":: performedBatches:" + performedBatches + " failedBatches:" + failedBatches + " totalWaitDuration:" + totalWaitDuration + (if (waiting) " waiting" else " running")
  }
}



/**
 * This represents a failing batch process (see [[SplittingBatchProcess]]) with multiple batch running in parallel.
 * @param m the simulation model
 * @param numberOfBatches the number of batches running in parallel.
 * @param batchDuration the duration of a batch starting from all inputs being inputted, and ending with the beginning of the outputting
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 * */
case class SplittingBatchProcess(m:Model,
                                 numberOfBatches:Int,
                                 batchDuration:() => Float,
                                 inputs:Array[(() => Int, Fetchable)],
                                 outputs:Array[Array[(()=>Int,Putable)]],
                                 name:String,
                                 transformFunction:ItemClass => (Int,ItemClass),
                                 verbose:Boolean = true) extends ActivableMultipleProcess(name,verbose){

  override val childProcesses:Iterable[SplittingSingleBatchProcess] =
    (1 to numberOfBatches) map((batchNumber:Int) => SplittingSingleBatchProcess(m,
      batchDuration,
      inputs,
      outputs,
      name + " chain " + batchNumber,
      transformFunction,
      verbose))

  override def toString: String = {
    name + ":: lines:" + numberOfBatches + " performedBatches: " + childProcesses.foldLeft(0)(_ + _.performedBatches) +
      " totalWaitDuration:" + childProcesses.foldLeft(0.0)(_ + _.totalWaitDuration) +
      " waitingLines:" + childProcesses.foldLeft(0)((waitings:Int,p:SplittingSingleBatchProcess) => waitings + (if (p.isWaiting) 1 else 0))
  }
}



/**
 *  A rolling (in a conveyor belt) Process means that if the output is blocked, no new batch is started
 * (imagine an industrial rolling band oven where croissants are cooked)
 * and if the input is blocked, the output still proceeds, (as if we were starting empty batches) there is no catch up for the waited time
 * batch only start when they ave their complete inputs.
 * if the output is blocked, the process stops, thus does not perform new inputs either (we cannot model that croissants will eventually burn in the oven)
 *
 * @param m the simulation model
 * @param processDuration the duration between inputting a batch and outputting the batch
 * @param minimalSeparationBetweenBatches the minimal separation between two consecutive batches
 * @param inputs the set of inputs (number of parts to input, storage)
 * @param outputs the set of outputs (number of parts, storage)
 * @param name the name of this process, for pretty printing
 * @param verbose true if you want to see the start input, start batch, end batch start output, end output events on the console
 * @author renaud.delandtsheer@cetic.be
 */
class ConveyorBeltProcess(m:Model,
                          processDuration:() => Float,
                          minimalSeparationBetweenBatches:Float,
                          val inputs:List[(() => Int, Fetchable)],
                          val outputs:List[(() => Int, Putable)],
                          transformFunction:ItemClass => ItemClass,
                          name:String,
                          verbose:Boolean = true) extends ActivableAtomicProcess(name,verbose){

  private val myOutput = new Outputter(outputs)
  override val myInput = new Inputter(inputs)

  //the belt contains the delay for the output since the previous element that was input. delay since input if the belt was empty
  private val belt: ListBuffer[(Double,ItemClass)] = ListBuffer.empty


  override def isRunning: Boolean = !blocked
  override def batchCount: Int = totalOutputBatches

  private var timeOfLastInput:Double = 0

  private var blocked:Boolean = false
  private var startBlockingTime:Double = 0
  private var outputMustBeRestarted:Boolean = true
  private var inputMustBeRestarted:Boolean = true

  private var blockedTimeSinceLastInput:Double = 0

  private var totalInputBatches = 0
  private var totalOutputBatches = 0
  private var mTotalBlockedTime:Double = 0.0
  restartInputtingIfNeeded()

  override def totalWaitDuration():Double = if (blocked) mTotalBlockedTime + m.clock() - startBlockingTime else mTotalBlockedTime

  override def toString: String = {
    name + " " + this.getClass.getSimpleName + ":: content:" + belt.size + " totalInputBatches:" + totalInputBatches + " totalOutputBatches:" + totalOutputBatches + " totalBlockedTime:" + totalWaitDuration + (if (blocked) " blocked" else " running")
  }

  private def restartInputtingIfNeeded(): Unit ={
    if (inputMustBeRestarted) {
      if(verbose) println(name + " restarting belt")
      inputMustBeRestarted = false
      startPerformInput()
    }
  }

  //on s'assure juste qu'il y aie assez de place depuis le dernier qu'on a mis dedans
  private def startPerformInput() {
    val timeForNextInput = timeOfLastInput + minimalSeparationBetweenBatches + blockedTimeSinceLastInput

    if (blocked) {
      if (verbose) println(name + " belt blocked at output, next input blocked")
      inputMustBeRestarted = true
    } else if (belt.isEmpty || timeForNextInput >= m.clock()) {
      //we can perform the input
      if (verbose) println(name + " start input")
      startBlockingTime = m.clock()
      myInput.performInput(finishedInputs)
    } else {
      //we cannot do the input; need to sleep a bit because belt was blocked from output
      val durationToNextInput = timeForNextInput - m.clock()
      m.wait(durationToNextInput){startPerformInput()}
    }
  }

  private def finishedInputs(i:ItemClass): Unit ={
    if (belt.isEmpty) {
      belt.prepend((processDuration(),transformFunction(i)))
    } else {
      belt.prepend((m.clock - timeOfLastInput,transformFunction(i)))
    }
    timeOfLastInput = m.clock()
    blockedTimeSinceLastInput = 0
    restartOutputtingIfNeeded()
    if (verbose) println(name + " input performed")
    totalInputBatches += 1
    m.wait(minimalSeparationBetweenBatches){startPerformInput()}
  }

  //called at input time, after belt has been fed again
  private def restartOutputtingIfNeeded(){
    if (outputMustBeRestarted) {
      require(belt.nonEmpty)
      outputMustBeRestarted = false
      m.wait(belt.last._1) {
        startPerformOutput(belt.last._2)
      }
    }
  }

  private def startPerformOutput(i:ItemClass) {
    blocked = true
    startBlockingTime = m.clock()
    if (verbose) println(name + " start outputting")
    myOutput.performOutput(i,finishedOutputs)
  }

  private def finishedOutputs(): Unit = {
    blocked = false
    blockedTimeSinceLastInput = m.clock() - startBlockingTime
    mTotalBlockedTime += blockedTimeSinceLastInput
    belt.remove(belt.size-1)
    if (verbose) println(name + " output performed")
    totalOutputBatches += 1
    restartInputtingIfNeeded()
    //otherwise, the belt is empty and the outputting process will be restarted by the next performed input.
    if (belt.nonEmpty) {
      m.wait(belt.last._1) {
        startPerformOutput(belt.last._2)
      }
    } else {
      outputMustBeRestarted = true
      if(verbose) println(name + " belt is empty")
    }
  }
}
