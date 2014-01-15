package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.CBLSIntVar

/**
 * this is an abstract class representing a resource.
 * the purpose is to abstract away conflict identification and other stuff that are specific to each type of resource
 *
 * @param planning
 */
abstract class Resource(planning:Planning, n:String) {
  val ResourceID = planning.addResource(this)
  def model = planning.model
  def maxDuration = planning.maxduration
  val name = Option(n) getOrElse s"Resource $ResourceID"

  /** the level of overshoot of the resource.
    * The higher, the more important it is to solve it first in the flattening
    */
  val overShoot:CBLSIntVar

  /** this method is called by the framework before starting the scheduling
    * put anything that needs to be done after instantiation here
    */
  def close()

  /** the first violation of the resource in time
    *
    * @return
    */
  def worseOverShootTime:Int

  /**you need to eject one of these to solve the conflict
    * this can be null if the problem is actually solved in between, or if the problem cannot be solved*/
  def conflictingActivities(t:Int):Iterable[Activity]

  def toAsciiArt(headerLength:Int):String
}

