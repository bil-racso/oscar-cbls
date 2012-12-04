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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.initialSolution


/**
 * Class to inherit to create a new heuristic initial solution.
 */
abstract trait Heuristic {
  val heuristicTimer = HeuristicTimer
}

import java.util.concurrent.Semaphore

/**
 * Timer to monitor the creation of the initial solutions.
 * The timer is static, and it's then shared for all heuristic.
 *
 * Info : It can be used by a graphical interface to display a progressBar.
 */
object HeuristicTimer{
  /**
   * Maintains the progression of an heuristic.
   */
  val heuristicTimer = new TimerAverage

  /**
   * Returns the actual progression of an heuristic in percent.
   * @return the progression in percent
   */
  def getPercentComplete = {heuristicTimer.getPercentComplete}

  /**
   * Set the actual progression of an heuristic.
   * @param p the actual progression.
   */
  def setPercentComplete(p:Int) {heuristicTimer.actualPercentComplete=p;unlock}

  /**
   * Lock the semaphore which is associated to the timer.
   */
  def lock {heuristicTimer.lock.acquire()}

  /**
   * Unlock the semaphore which is associated to the timer.
   */
  def unlock {heuristicTimer.lock.release()}
}

/**
 * Maintains a progression in percent.
 */
class TimerAverage {
  var lock:Semaphore = new Semaphore(0)
  var actualPercentComplete:Int = 0
  def setPercentComplete(p:Int) {actualPercentComplete=p}
  def getPercentComplete:Int = actualPercentComplete
}