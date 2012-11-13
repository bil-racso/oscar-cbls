package oscar.cbls.routing.heuristic

import java.util.concurrent.Semaphore

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
/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 8/11/12
 * Time: 22:36
 * To change this template use File | Settings | File Templates.
 */

object HeuristicTimer{
  val heuristicTimer = new TimerAverage

  def getPercentComplete = {heuristicTimer.getPercentComplete}
  def setPercentComplete(p:Int) {heuristicTimer.actualPercentComplete=p}
  def lock {heuristicTimer.lock.acquire()}
  def unlock {heuristicTimer.lock.release()}
}

class TimerAverage {
  var lock:Semaphore = new Semaphore(0)
  var actualPercentComplete:Int = 0
  def setPercentComplete(p:Int) {actualPercentComplete=p}
  def getPercentComplete:Int = actualPercentComplete
}