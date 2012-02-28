/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package scampi.des.engine

import scala.util.continuations._
import scampi.invariants._

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