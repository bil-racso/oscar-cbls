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
//import scala.react._

class Frequency(m: Model, state: State[_]){

  var duration:Double = 0
  var last:Double = 0
//  
//  Reactor.loop{self=>
//    val last = m.time(self next state.atEntry)
//    duration += m.time(self next state.atLeaving) - last
//  }
  
  def apply() = duration/m.clock()
  
}