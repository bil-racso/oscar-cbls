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

package oscar.cbls.routing.test

import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.routing.model._
/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 17/10/12
 * Time: 14:12
 * To change this template use InstanceVRP | Settings | InstanceVRP Templates.
 */


object DebugFlipAndReverse extends App{
  val m: Model = new Model(false,true,false,false)
  val vrp =  new VRP (9,1,m)

  // make a cycle 0-1-2-3-4-5-6-0 (circle)
  vrp.Next(0):=1
  vrp.Next(1):=2
  vrp.Next(2):=3
  vrp.Next(3):=4
  vrp.Next(4):=5
  vrp.Next(5):=6
  vrp.Next(6):=7
  vrp.Next(7):=8
  vrp.Next(8):=0
  println("VRP:\n"+vrp)

  //easy flip the circle
  /*
  vrp.reverse(0,6).foreach(t => t._1 := t._2)
  vrp.Next(0):=6
  */

  //vrp.flip(2,3,5,6).foreach(t => t._1 := t._2)
  //println("VRP after flip:\n"+vrp)


  //vrp.threeOptC(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  vrp.threeOptB(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  println("VRP after flip:\n"+vrp)

}