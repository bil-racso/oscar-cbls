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

import scampi.invariants._
import scala.util.continuations._

object QueueState extends Enumeration{
    type QueueState = Value
    val empty, serving, closed = Value
  }

class SimQueue(m: Model) extends Resource(m, 1){

  
  import QueueState._
  
  val state = new Var[QueueState](m,closed)
  
  
  def enter = {
    if ( state is closed ) false
    else{
      if ( state is empty) state := serving
      request
      true    
    }
  }
  
  override def release(){
    super.release()
    if ( isEmpty && ! (state is closed) ) state := empty    
  }
  
  def close(){state := closed}
  def open(){state := empty}
  
}