/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.des.engine

import scala.Math._

import oscar.stochastic._
import scala.util.Random
import scala.util.continuations._
import JSci.maths.statistics._
import oscar.invariants._
import org.scala_tools.time.Imports._

object Generator {
    def forever[T](dist: Distr[Double])(block: => Unit)(implicit m: Model[T]) = new Generator(m, dist)(
      {block
      true}
    )
	def apply[T](dist: Distr[Double])(block: => Boolean)(implicit m: Model[T]) = new Generator(m, dist)(block)
	def apply(dist: ProbabilityDistribution) = new NumberGenerator(dist)
}

class Generator[T](m: Model[T], var dist: Distr[Double])(block: => Boolean) extends ProcessUnit[T]("Generator")(m){

  def restart() ={
    if (!generating) {
      start()
    }
  }
  def start() = {
    generating = true
      while (generating) {
        val t = floor(dist.apply(m)).toLong
        val a = w(m.clock === new DateTime(m.clock().getMillis() + t) )
        if (generating){
          if(!block) generating = false
        }
      }
  }

  var generating = true 
  
  def stop() { generating = false }
  def update(v: Distr[Double]) {
    dist = v
  }
}
class NumberGenerator(dist: ProbabilityDistribution) {

  val generator = new Random()

  var generating = true

  def stop() { generating = false }

  def apply(): Double = dist.inverse(generator.nextDouble)
  def generateNext = apply

}

