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
 ******************************************************************************/
package oscar.des.engine

import scala.util.continuations._
import oscar.invariants._

class Tank(m: Model, capacity: Double) {

  val load = new Var[Double](0.0)  

  def get(qty: Double): Unit @suspendable = {
    waitFor( load.filter(_ >= qty) )
    load := load - qty
  }

  def put(qty: Double): Unit @suspendable = {

    waitFor(load.filter( _ <= capacity-qty) )
    load := load + qty
  }
}
