package oscar.cp.memScheduling

import scala.collection.mutable.ListBuffer
import oscar.cp.core._
import scala.util.continuations._
import scala.collection.mutable.Map
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt

trait Scheduler {
  implicit val defaultSchedulerIndex = 0
  val multiSchedulers = Array[CPScheduler](CPScheduler(Int.MaxValue))
  
  implicit def scheduler(implicit index: Int): CPScheduler = multiSchedulers(index)
  def cp(implicit index: Int) = scheduler(index)
  
  def horizon(implicit index: Int) = scheduler(index)
  implicit def cp2Horizon(cp: CPScheduler): Int = cp.horizon
  
  implicit def List2schedStore(multiSchedulers: Array[CPScheduler]) = new {
  	def use(nbSched: Int) {
  	  multiSchedulers :+ List.fill(nbSched-multiSchedulers.length)(CPScheduler(Int.MaxValue))
  	}
  }
}