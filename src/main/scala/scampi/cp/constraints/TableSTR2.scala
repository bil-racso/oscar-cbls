/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package scampi.cp.constraints
import scampi.cp.core._
import scampi.reversible._
import scampi.cp.core.CPOutcome
import scampi.cp.modeling._
import scala.collection.JavaConversions._


/**
 * Implementation of this algorithm:
 * STR2: Optimized Simple Tabular Reduction for Table Constraints.
 * Constraints, Volume 16, Number 4.
 * Pages 341-371, Springer.
 * October 2011.
 * 
 * @author Jean-Baptiste Mairy and Pierre Schaus (pschaus@gmail.com)
 */
class TableSTR2(val X: Array[CPVarInt], table: Array[Array[Int]]) extends Constraint(X(0).getStore(), "Table2") {

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
    setIdempotent()
    if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    X.filter(!_.isBound()).foreach(_.callPropagateWhenDomainChanges(this))
    return CPOutcome.Suspend
  }
  
  private def init() {
    sval.empty()
    sup.empty()
  }
  
  private def changed(i: Int) = lastSize(i).value != X(i).getSize()

  
  def isValid(t: Int): Boolean = {
    val tuple = table(t) 
    ! sval.exists(i => !X(i).hasValue(tuple(i)))
  }

  override def propagate(): CPOutcome = {
    init()

    for ((x,i) <- X.zipWithIndex) {
       if (changed(i)) sval.insert(i)
       /*if (!x.isBound())*/ sup.insert(i) 
    }
    
    // retrieve domains in sets
    import scala.collection.mutable.Set
    
    val toRemoveValues = Array.tabulate(X.size)(i => Set((X(i).getMin() to X(i).getMax).filter(X(i).hasValue(_)) : _*))
    
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
    for ((x,i) <- X.zipWithIndex) { lastSize(i).setValue(x.getSize()) }
    return CPOutcome.Suspend
  }

}


object TableSTR2 extends CPModel {
  def main(args: Array[String]) {
	  val tuples = Array(Array(1,2,3),Array(2,2,3),Array(3,2,1))
	  val cp = CPSolver()
	  var X = Array.fill(3)(CPVarInt(cp,0 to 5))
	  cp.add(new TableSTR2(X,tuples))
	  println(X.mkString(" - "))
  }
}
