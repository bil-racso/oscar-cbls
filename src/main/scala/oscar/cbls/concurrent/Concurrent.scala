/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Drobisz Sébastien
  ******************************************************************************/
package oscar.cbls.concurrent

import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import oscar.cbls.invariants.core.propagation.{PropagationStructure, PropagationElement}

class Worker(val index:Int, val pool:ThreadPool) extends Runnable {
  private var job:Job = null
  private var end = false
  private val sem = new Semaphore(0)

  // the final declaration is use to allow the compiler to optimize recursion in loop
  @tailrec final def run() {
    lock()
    if(!end) {
      job.execute()
      pool.freeThread(index)
      run()
    }
  }
  def setJob(that:Job) {this.job = that}
  def doJob() {unlock()}
  def makeEnd() {
    end = true
    unlock()
  }
  private def lock() {sem.acquire()}
  private def unlock() {sem.release()}
}

class ThreadPool(val nbThread:Int = 2) {
  private val threads = new Array[Thread](nbThread)
  private val workers = new Array[Worker](nbThread)
  private val semEmpty = new Semaphore(nbThread)
  private val semAccess = new Semaphore(1)
  private var unusedThread = List[Int]()
  initThreadAndWorkers()

  private def initThreadAndWorkers() {
    for(i <- 0 until nbThread) {
      workers(i) = new Worker(i, this)
      threads(i) = new Thread(workers(i))
      threads(i).setPriority(Thread.MIN_PRIORITY)
      unusedThread = i :: unusedThread
    }
  }

  def getWorkers:Array[Worker] = workers

  def getIndexOfAvailableThread:Int = {
    lockEmptyList()

    lockAccessList()
    val threadID = unusedThread.head
    unusedThread = unusedThread.tail
    unlockAccessList()

    threadID
  }

  def freeThread(index:Int) {
    lockAccessList()
    unusedThread = index :: unusedThread
    unlockAccessList()

    unlockEmptyList()
  }

  /**
   * The barrier method could be use to wait that every worker finish its work
   */
  def barrier() {
    lockFullList()
    unlockFullList()
  }

  def getNbAvailableThread:Int = {
    unusedThread.size
  }

  def size = nbThread

  def start() {
    for(t <- threads) t.start()
  }
  def stop() {
    barrier()
    for(w <- workers) w.makeEnd()
    for(t <- threads) t.join()
  }

  private def lockEmptyList() {semEmpty.acquire()}
  private def unlockEmptyList() {semEmpty.release()}
  private def lockAccessList() {semAccess.acquire()}
  private def unlockAccessList() {semAccess.release()}
  private def lockFullList() {semEmpty.acquire(nbThread)}
  private def unlockFullList() {semEmpty.release(nbThread)}
}

trait Job {
  def execute()
}

class NodePropagator(pe:PropagationElement) extends Job {
  def execute() {pe.propagate()}
}

class Dispatcher(nbThread:Int=2) {
  private val pool = new ThreadPool(nbThread)
  pool.start()
  private val workers = pool.getWorkers

  def propagateElements(elements: List[PropagationElement]) {
    var tmp = elements
    while(!tmp.isEmpty) {
      val indexOfAvailableWorker = pool.getIndexOfAvailableThread
      workers(indexOfAvailableWorker).setJob(new NodePropagator(tmp.head))
      workers(indexOfAvailableWorker).doJob()
      tmp = tmp.tail
    }
  }

  /**
   * This method allow the dispatcher to distribute the given jobs to
   * the free thread. If no thread is available to do a job, we wait
   * a thread finished its previous job.
   * @param jobs The given jobs to distribute
   */
  def doJob(jobs:List[Job]) {
    var listOfJob = jobs
    while(!listOfJob.isEmpty) {
      val indexOfAvailableWorker:Int = pool.getIndexOfAvailableThread
      workers(indexOfAvailableWorker).setJob(listOfJob.head)
      workers(indexOfAvailableWorker).doJob()
      listOfJob = listOfJob.tail
    }
  }

  /**
   * This method distributes the given job to the given number of threads
   * This job have to be concurrent safe to prevent conflict like race competition
   * or deadlock.
   * @param job Job to do
   * @param toNbThread Number of thread that should do the given job
   */
  def doJob(job:Job, toNbThread:Int=nbThread) {
    for (i <- 1 to toNbThread) {
      val availableThreadIndex = pool.getIndexOfAvailableThread
      workers(availableThreadIndex).setJob(job)
      workers(availableThreadIndex).doJob()
    }
  }

  /**
   * This method wait the given job to this dispatcher is finished
   */
  def waitIdleStatus() {pool.barrier()}

  /**
   *  This method stop the dispatcher and wait that its given job is finished.
   *  If a dispatcher is stop, it can't be use further.
   */
  def stop() {pool.stop()}
}

trait PropagationStructureSynch  extends PropagationStructure with Job {
  private val availableProcessors = Runtime.getRuntime.availableProcessors()
  private val nbThread = availableProcessors
  private val dispatcher = new Dispatcher(nbThread * 2)
  private var Track:Array[Boolean] = null

  private val semAccess = new Semaphore(1)
  private val barrierSem = new Semaphore(nbThread)
  private var l:List[PropagationElement] = List.empty
  private def lock() {semAccess.acquire()}
  private def unlock() {semAccess.release()}
  private def lockBarrier() {barrierSem.acquire()}
  private def unlockBarrier() {barrierSem.release()}

  def getDispatcher = dispatcher

  private def barrier() {
    barrierSem.acquire(nbThread)
    barrierSem.release(nbThread)
  }

  @tailrec final def execute() {
    var nodeToPropagate:PropagationElement = null

    lock() // This lock is use to prevent two threads to access lists at the same time
    if(l.isEmpty) {
      barrier() // the purpose of this barrier is waiting each thread finishes to propagate the lasts PEs of the current level

      for (e <- ScheduledElements) {
        if (Track == null || Track(e.UniqueID)) {
          ExecutionQueue.insert(e)
        } else {
          PostponedComponents = e :: PostponedComponents
        }
      }
      ScheduledElements = List.empty

      if(!ExecutionQueue.isEmpty)
        l = ExecutionQueue.popFirsts
    }

    if(!l.isEmpty) {
      nodeToPropagate = l.head
      l = l.tail
    }
    unlock()

    if(nodeToPropagate != null) {
      lockBarrier()
      nodeToPropagate.propagate()
      unlockBarrier()

      execute()
    }
  }

  override def propagateOnTrack(Track: Array[Boolean], SameAsBefore: Boolean) {
    if (Propagating) return
    Propagating = true

    this.Track = Track

    if (!SameAsBefore) {
      var NewPostponed: List[PropagationElement] = List.empty
      for (e: PropagationElement <- PostponedComponents) {
        if (Track == null || Track(e.UniqueID)) {
          ScheduledElements = e :: ScheduledElements
        } else {
          NewPostponed = e :: NewPostponed
        }
      }
      PostponedComponents = NewPostponed
    } //if it is SameAsBefore, we do not check whether the elements are in the track,
    // as they are postponed, they are not in it anyway
    //notice that for partial propagation, connex components cannot be partially propagated
    // because they are strognly connected over the static propagation graph.

    if (Verbose) {
      if (Track == null) println("PropagationStruture: start total propagation")
      else println("PropagationStruture: start partial propagation")
    }

    for (e: PropagationElement <- ScheduledElements) {
      if (Track == null || Track(e.UniqueID)) {
        ExecutionQueue.insert(e)
      } else {
        PostponedComponents = e :: PostponedComponents
      }
    }
    ScheduledElements = List.empty

    dispatcher.doJob(this, 2) //TODO
    dispatcher.waitIdleStatus()

    if (Verbose) println("PropagationStruture: end propagation")

    if (DebugMode && Track == null) {
      for (p <- getPropagationElements) {
        p.checkInternals()
      }
    }
    Propagating = false
  }

  def emptyScheduledElements() {
    ScheduledElements = List.empty
  }

  val semSchedule = new Semaphore(1)
  override def scheduleForPropagation(p: PropagationElement) {
    semSchedule.acquire()
    ScheduledElements = p :: ScheduledElements
    semSchedule.release()
  }

  override def endPropagation() {dispatcher.stop()}
}

class SemaphoreTH(permits:Int = 1) {
  var id = -1
  var cpt = 0
  val sem = new Semaphore(permits)
  def acquire(that:Int, permits:Int=1) {
    if(that == id) {
      cpt+=1
    }
    else {
      sem.acquire(permits)
      id = that
    }
  }
  def release(that:Int, permits:Int=1) {
    if(id == that) {
      if(cpt == 0)
        sem.release(permits)
      else
        cpt -=1
    }
  }
}