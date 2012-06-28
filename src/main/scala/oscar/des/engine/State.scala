/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package oscar.des.engine

import scala.util.continuations._
import oscar.invariants._

class BasicState{}
class State[A] extends BasicState {
  

  val atEntry = new Event[A]
  val atLeaving = new Event[A]
  
  def code(param: A): Unit @suspendable={println()}
  
  def run(param:A): Unit@suspendable = {
    atEntry emit param
    code(param)
    atLeaving.emit(param)
  }
}
