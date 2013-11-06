package oscar.cp.memScheduling

import oscar.cp.modeling.CPScheduler
import scala.collection.mutable.ListBuffer
import oscar.cp.core.CPVarInt
import oscar.cp.scheduling.Activity
import scala.util.continuations._

trait Scheduler {  
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
	
}