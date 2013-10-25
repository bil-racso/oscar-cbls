package oscar.cp.memScheduling

import scala.collection.mutable.ListBuffer
import oscar.cp.core._
import oscar.cp.scheduling._
import scala.util.continuations._
import oscar.cp.modeling._
import scala.collection.mutable.Map
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt

trait Scheduler {
  implicit val scheduler = CPScheduler(Int.MaxValue)
  
  val horizon 					= scheduler
  //val objective		 			= scheduler
  //val exploration 			= scheduler
  
  implicit def cp2Horizon(cp: CPScheduler): Int = cp.horizon
  
  /*
  val cpStore = ListBuffer[CPScheduler](CPScheduler(Int.MaxValue))
  
  implicit val defaultSchedulerIndex = 0
  implicit val defaultScheduler:CPScheduler = cp
  
  def cp(implicit index: Int): CPScheduler = {
    cpStore(index)
  }
  
  def setHorizonTo(horizon: Int)(implicit schedulerIndex: Int) {
    cpStore(schedulerIndex).horizon = horizon
  }
  
  def minimize(toMinimize : CPVarInt)(implicit scheduler : CPScheduler): CPScheduler = {
  	scheduler.minimize(toMinimize)
  	scheduler
  }
  
  def binary(vars: Array[_ <: CPVarInt])(implicit scheduler: CPScheduler): Unit @suspendable = {
    scheduler.binary(vars)
  }
  
  def printStats()(implicit scheduler: CPScheduler) {
    scheduler.printStats()
  }
	*/
}