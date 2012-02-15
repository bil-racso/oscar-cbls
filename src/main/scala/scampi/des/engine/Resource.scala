/**
 * *****************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *
 * Contributors:
 *      www.n-side.com
 * ****************************************************************************
 */
package scampi.des.engine

import scala.collection.mutable._
import scala.util.continuations._
import scala.react._

/**
 * Capacitated resource where waiting customers are served in FIFO order
 * @author pschaus
 */
class Resource(m: Model, capacity: Int) {

  private val n = new Var(0)

  def request(): Unit @suspendable = {
    m.waitFor[Int](n, _ < capacity)
    n.update(n() + 1)
    println(2)
  }

  def release() {
    n.update(n() - 1)
  }

}

class UnaryResource(m: Model) extends Resource(m, 1)