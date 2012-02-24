package scampi.cp.constraints
import scampi.cp.core.Constraint
import scampi.cp.core._
import scampi.reversible._
import scampi.cp.core.CPOutcome
import scampi.cp.modeling._

/**
 * Global Cardinality Constraint
 *
 * Constraint the values minval+i to appear between low[i] and up[i] times in x
 * @param x
 * @param minval
 * @param low
 * @param up
 * @see SoftGCC
 * @see GCCVar
 *
 * @author Pierre Schaus pschaus@gmail.com and Bertrand Cornelusse bcr@n-side.com
 */
class GCCFWC(val X: Array[CPVarInt], minVal: Int, low: Array[Int], up: Array[Int]) extends Constraint(X(0).getStore(), "GCCFWC") {

  val nbBound = Array.tabulate(low.size)(i => new ReversibleInt(X(0).getStore, 0))
  val constrainedValues = minVal until minVal + low.length toArray

  /**
   * Initialization, input checks and registration to events
   */
  override def setup(l: CPPropagStrength): CPOutcome = {
    var outcome: CPOutcome = CPOutcome.Suspend

    // Input checks
    assert(low.length == up.length)
    for (i <- 0 until low.length) {
      assert(!(low(i) > up(i)))
    }

    outcome = propagate()
    if (outcome == CPOutcome.Suspend) {
      for (i <- 0 until X.length) {
        X(i).callPropagateWhenDomainChanges(this);
        X(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      }
    }

    outcome
  }

  override def propagate(): CPOutcome = {
    var outcome: CPOutcome = CPOutcome.Suspend

    println("something changed:" + X.mkString(","));

    // Set of variables that have at least one value in the set of cardinality constrained values 
    var X2 = Set[scampi.cp.core.CPVarInt]()
    constrainedValues foreach (v => X2 = X2 union (X filter (x => x.hasValue(v)) toSet))

    // Subset of X2 such that each variable has some values out of the set of cardinality constrained values 
    var X3 = X2 filter (x => x.getMin() < constrainedValues.first || x.getMax() > constrainedValues.last)

    // Process values one by one

    val skipList = Array.tabulate(low.length)(x => true) // Change skipList(i) to false if you do not wan to consider it 
    var restart = true // Restart the loop each time a domain is changed...
    while (outcome != CPOutcome.Failure && restart) {
      restart = false
      
      for {i <- 0 until low.length; if (skipList(i))} {
        // i is the index of the value to process
        
        val candidates = X2 filter (_.hasValue(constrainedValues(i))) toSet
        val boundCandidates = candidates filter (x => x.isBound())
        val nonBoundCandidates = candidates diff boundCandidates
        val nbCandidates = candidates.size

        if (nbCandidates < low(i)) {
          // Not enough candidates to satisfy ith cardinality constraint
          outcome = CPOutcome.Failure
        } else if (nbCandidates == low(i)) {
          // Exactly enough candidates to satisfy ith cardinality constraint
          for (x <- nonBoundCandidates) {
            x.assign(constrainedValues(i)) //FIXME (when) will propagate be called? 
            restart = true
            skipList(i) = false // Stop considering this value in the loop
          }
        } else if (boundCandidates.size >= low(i) && candidates.size < up(i)) {
          skipList(i) = false // Stop considering this value in the loop
        } else if (boundCandidates.size == up(i)) {
          for (x <- nonBoundCandidates) {
            x.removeValue(constrainedValues(i))
            restart = true
            skipList(i) = false // Stop considering this value in the loop
          }
        } else if (boundCandidates.size > up(i)) {
          // Too many candidates to satisfy ith cardinality constraint
          outcome = CPOutcome.Failure
        } else {
          // Can we do sth locally?              
        }

        // Reevaluate X2 and X3
        constrainedValues foreach (v => X2 = X2 union (X filter (x => x.hasValue(v)) toSet))
        X3 = X2 filter (x => x.getMin() < constrainedValues.first || x.getMax() > constrainedValues.last)
      }
    }

    // Check overall feasibility after potential changes
    if (low.sum > X2.size) {
      println("Too few variables to satisfy the lower bounds of CC")
      outcome = CPOutcome.Failure
    } else if (X2.size - up.sum > X3.size) {
      println("Too many variables to satisfy the upper bounds of CC")
      outcome = CPOutcome.Failure
    }

    val candidatesByValue = constrainedValues map (v => X filter (_.hasValue(v)) toList) toArray

     println("something changed:" + X.mkString(","));

    outcome
  }

  override def valRemoveIdx(x: CPVarInt, i: Int, v: Int): CPOutcome = {
    println("var at index " + i + " lost value " + v)
    CPOutcome.Suspend
  }
}

object GCCFWC {

  def main(args: Array[String]) {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    // T1 
    //    	  cp.add(new GCCFWC(x,0,Array(0,2,0,2),Array(3,2,3,2)))
    // Too few variables to satisfy the lower bounds of CC

    // T2
    //    	  cp.add(new GCCFWC(x,0,Array(0,0,0,0),Array(2,0,0,0)))
    // Too many variables to satisfy the upper bounds of CC

    // T3 variation of T2
    //    cp.add(new GCCFWC(x,0,Array(0,0,0),Array(2,0,0)))
    // --> Candidates by value: List( 0,  [0, 3]),List(),List()

    // T4
    cp.add(new GCCFWC(x, 0, Array(0, 1, 0, 2), Array(3, 2, 3, 2)))

    // T5 
    //    cp.add(new GCCFWC(x,0,Array(0,0,0,2),Array(0,2,3,2)))

    // T6
    // cp.add(new GCCFWC(x,0,Array(0,0,0,0),Array(1,1,1,1)))

    //cp.add(x1 != 0)

  }
}
