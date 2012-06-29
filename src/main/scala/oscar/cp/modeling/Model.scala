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
package oscar.cp.modeling


import scala.util.continuations._
import scala.collection.IterableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom



/**
 *
 * A CP model should extend this class to benefit from the modeling facilities (constraints functions...)
 *
 */
trait CPModel extends Constraints {
  
  import oscar.cp.core._
  import oscar.cp.search._
  import oscar.search._
  import oscar.cp.constraints._
  import oscar.cp.core.CPVarInt
  import oscar.cp.core.CPVarBool
  import oscar.search.Branching._

  /**
   * Filtering power can be specified for some of the constraints.
   * The default filtering is Weak.
   */
  val Strong = CPPropagStrength.Strong
  val Medium = CPPropagStrength.Medium
  val Weak = CPPropagStrength.Weak
  

  

  implicit def convert2(vals: IndexedSeq[Int]) = vals.toArray[Int]

  
  implicit def indexed2Array(x: IndexedSeq[CPVarInt]) = x.toArray[CPVarInt]
  implicit def args2Array(x: CPVarInt*) = x.toArray[CPVarInt]

  
  implicit def indexed2ArrayBool(x: IndexedSeq[CPVarBool]) = x.toArray[CPVarBool]
  implicit def args2ArrayBool(x: CPVarBool*) = x.toArray[CPVarBool]

  
  
  //implicit def convertSeqVars2ArrayVars[T <: CPVarInt](x: scala.collection.immutable.IndexedSeq[T]) : Array[T]= x.toArray
  
  implicit def richIterable[A,Repr](xs: SeqLike[A,Repr]) = new { 
	def suspendable = new {
		def foreach(yld: A => Unit @suspendable): Unit @suspendable = {	
				loop(xs.indices) {
				  i => yld(xs(i))
				}
		}
	}
  }
  
  def loopWhile[T](cond: =>Boolean)(body: =>(Unit @suspendable)): Unit @suspendable = {
     if (cond) {
       body
       loopWhile[T](cond)(body)
     } 
   }
    
   def loop(r: Range)(body: Int =>(Unit @suspendable)): Unit @suspendable = {
      var i = r.start
      loopWhile(i < r.end) {
        val k = i
        body(i)
        i = k+1
      }
   }
  


  /**
   * @param block a code block
   * @return the time (ms) to execute the block
   */
  def getTime(block: => Unit): Long = {
    val t0 = System.currentTimeMillis();
    block
    System.currentTimeMillis - t0;
  }
  



  //helper functions

  /**
   * @param vars an array of CPVarInt
   * @return an array of tuple (variable,index of variables in vars) composed
   *         of the not-bound variables (at least two values in the domain)
   *         having the smallest domain size. an empty array if every variable is bound
   */
  def minDomNotbound(vars: Iterable[CPVarInt]): Iterable[(CPVarInt, Int)] = {
    val notbound = vars.filterNot(_.isBound)
    if (notbound.nonEmpty) {
      val sizeMin = notbound.map(_.getSize).min
      notbound.zipWithIndex.filter {
        _._1.getSize == sizeMin
      }
    } else {
      Iterable()
    }
  }
  
  def allBounds(vars: Iterable[CPVarInt]) = vars.forall(_.isBound())
  
  def argMax[A](indexes: Iterable[A])(f: A => Int): Iterable[A] = {
    val max = indexes.map(f).max
    indexes.filter(f(_) == max)
  }
  
  def argMax2[A](indexes: Iterable[A])(f1: A => Int, f2: A => Int): Iterable[A] = {
    val maxf1 = indexes.map(f1).max
    val tmp = indexes.filter(f1(_) == maxf1)
    val maxf2 = tmp.map(f2).max
    tmp.filter(f2(_) == maxf2)
  }
  
  def argMin[A](indexes: Iterable[A])(f: A => Int): Iterable[A] = {
    val min = indexes.map(f).min
    indexes.filter(f(_) == min)
  }
  
  object CPVarInt {

  /**
   * Creates a new CP Integer Variable with a range as initial domain
   * @param cp the solver in which the variable is created
   * @param domain the range defining the possible values for the variable
   * @return a fresh CPVarInt defined in the solver cp with initial domain {domain.min,, ..., domain.max}
   */
  def apply(cp: CPSolver, domain: Range): CPVarInt = {
    new CPVarInt(cp, domain)
  }

  /**
   * Creates a new CP Integer Variable instantiated to a value
   * @param cp the solver in which the variable is created
   * @param value is the value to which the variable is instantiated
   * @return a fresh CPVarInt defined in the solver cp with initial domain {value}
   */
  def apply(cp: CPSolver, value: Int): CPVarInt = {
    new CPVarInt(cp, value, value)
  }

  /**
   * Creates a new CP Integer Variable with a set of values as initial domain
   * @param cp the solver in which the variable is created
   * @param values is the initial set of values possible for the variable (domain)
   * @return a fresh CPVarInt defined in the solver cp with initial domain equal to the set of values
   */
  def apply(cp: CPSolver, values: Set[Int]): CPVarInt = {
    val vals = new java.util.HashSet[Integer]()
    values.foreach(v => vals.add(v))
    new CPVarInt(cp, vals)
  }
  }

  object CPVarBool {

   /**
    * Creates a new CP Boolean Variable
    */
   def apply(cp: CPSolver): CPVarBool = {
    new CPVarBool(cp)
   }
  }

}

