package oscar.cp.scheduling2.tasks

import oscar.cp.core.CPVarInt
import oscar.cp.scheduling2.resources.Resource
import scala.collection.mutable.Map

/** @author Renaud Hartert ren.hartert@gmail.com **/

abstract class Task(
  val start: CPVarInt,      // starting time of the task
  val duration: CPVarInt,   // duration of the task
  val end: CPVarInt,        // ending time of the task
  val name: String = "Task" // Name of the task
) {
  
  private val subTasks: Map[Resource, ResourceTask] = Map()

  /** Returns the store of the task **/
  def store = start.store

  /** Returns true if the task is using the resource, false otherwise **/
  def isUsing(resource: Resource): Boolean = subTasks.contains(resource)

  def consumes(resource: Resource): CPVarInt = {
    subTasks.get(resource) match {
      case None => throw new Exception("Not defined on this resource")
      case Some(task) => task.height
    }
  }

  def needs(resource: Resource)

  def gives(resource: Resource)

  def needsF(resource: Resource)

  def givesF(resource: Resource)
}