/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.constraints.implementations
import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome
import oscar.cp.modeling._
import scala.collection.JavaConversions._
import scala.Array.canBuildFrom
import scala.collection.mutable.Set


/**
 * Implementation of this algorithm:
 * STR2: Optimized Simple Tabular Reduction for Table Constraints.
 * Constraints, Volume 16, Number 4.
 * Pages 341-371, Springer.
 * October 2011.
 * 
 * @author Jean-Baptiste Mairy and Pierre Schaus (pschaus@gmail.com)
 */
class TableSTR2(val X: Array[CPVarInt], table: Array[Array[Int]]) extends Constraint(X(0).s, "Table2") {

  val lastSize = Array.fill(X.size)(new ReversibleInt(s,-1))
  
  
  val validTuples = new ReversibleSetIndexedArray(s, 0, table.size-1)
  
  // the set of uninstantiated variables whose domain has been reduced since the previous invocation
  // initially, this set also contains the last assigned variable
  val sval = new SetIndexedArray(0, X.size-1)
  
  // the set of uninstantiated variables whose domain contains at least one value for which a support has not yet been found.
  val sup = new SetIndexedArray(0, X.size-1) 
  
  def invalidate(tupleInd: Int) {
    validTuples.removeValue(tupleInd)
  }

  /**
   * Initialization, input checks and registration to events
   */
  override def setup(l: CPPropagStrength): CPOutcome = {    
    idempotent = true
    if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    return CPOutcome.Suspend
  }
  
  private def init() {
    sval.empty()
    sup.empty()
  }
  
  private def changed(i: Int) = lastSize(i).value != X(i).size

  
  def isValid(t: Int): Boolean = {
    val tuple = table(t) 
    ! sval.exists(i => !X(i).hasValue(tuple(i)))
  }

  override def propagate(): CPOutcome = {
    init()

    for ((x,i) <- X.zipWithIndex) {
       if (changed(i)) sval.insert(i)
       /*if (!x.isBound)*/ sup.insert(i) 
    }
    
    // retrieve domains in sets
    import scala.collection.mutable.Set
    
    val toRemoveValues = Array.tabulate(X.size)(i => Set((X(i).min to X(i).max).filter(X(i).hasValue(_)) : _*))
    
    val toRemoveFromTuples = Set[Integer]() // used to avoid removing while iterating in validTuples
    
    for (t <- validTuples) {  
      val tuple = table(t)
      if (isValid(t)) { // check if tuple is valid wrt changed variables
        val toRemoveFromSup = for (i <- sup; if (toRemoveValues(i).remove(tuple(i)) && toRemoveValues(i).isEmpty)) yield i
        toRemoveFromSup.foreach(sup.removeValue(_))
      } else {
        toRemoveFromTuples += t
      }
    }
    toRemoveFromTuples.foreach(validTuples.removeValue(_))
    
    for (i <- sup; v <- toRemoveValues(i)) {
      if (X(i).removeValue(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }
    // update sizes
    for ((x,i) <- X.zipWithIndex) { lastSize(i).setValue(x.size) }
    return CPOutcome.Suspend
  }

}


object TableSTR2 {
  def main(args: Array[String]) {
	  val tuples = Array(Array(1,2,3),Array(2,2,3),Array(3,2,1))
	  val cp = CPSolver()
	  var X = Array.fill(3)(CPVarInt(0 to 5)(cp))
	  cp.add(new TableSTR2(X,tuples))
	  println(X.mkString(" - "))
  }
}
