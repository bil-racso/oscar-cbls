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
import scala.annotation._

import scala.util.control.TailCalls._

abstract class State[A](m: Model) {
  
  
	val isIn = new Var[Boolean](m, false)
  
   def code(param: A): Unit @suspendable
  
  def apply(param:A)={
    isIn := true
        code(param)
    isIn := false
  }
}

object State{
  type state[A] = TailRec[A] 
  def done[A](a: A):state[A] = done(a)
}

//abstract class StateOne[A](m: Model) {
//  
//	val isIn = new SignalOne[Boolean](m, false)
//  
//  def code(param: A): Unit @suspendable
//  
//  def apply(param:A): Unit@suspendable = {
//    isIn set( true )
//    code(param)
//    isIn set false
//  }
//}