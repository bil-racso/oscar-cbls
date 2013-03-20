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

package oscar.cp.scheduling

/**
 * @param s = start
 * @param e = end
 * @param i = consumption
 * @param m = machine
 */
class FixedActivity(idx : Int, private var s : Int, 
							   private var e : Int, 
							   private var i : Int, 
							   private var m : Int) {

	def start = s
	def start_= (x : Int) {s = x}
	
	def end = e
	def end_= (x : Int) {e = x}
	
	def inc     = i
	def inc_= (x : Int) {i = x}
	
	def machine = m
	def machine_= (x : Int) {m = x}
	
	def id      = idx
	
	override def toString() = "<id: " + idx + ", start: " + s + ", end: " + e + ", machine: " + m + ", inc: " + i + ">"
}
