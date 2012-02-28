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
class GCCFWC(val X: Array[CPVarInt], val minVal: Int, val low: Array[Int], val up: Array[Int]) extends Constraint(X(0).getStore(), "GCCFWC") {

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
      }
    }

    outcome
  }

  /**
   * TODO Ensure idempotent
   * TODO Persistence and ReversibleInt
   * TODO Implement other listeners
   * TODO Efficiency, dataStructures
   */
  override def propagate(): CPOutcome = {
    var outcome: CPOutcome = CPOutcome.Suspend

    // DEBUG 
    //    println("something changed:" + X.mkString(","));

    // Set of variables that have at least one value in the set of cardinality constrained values 
    var X2 = Set[scampi.cp.core.CPVarInt]()
    constrainedValues foreach (v => X2 = X2 union (X filter (x => x.hasValue(v)) toSet))

    // Process constrained values one by one
    val skipList = Array.tabulate(low.length)(x => true) // Change skipList(i) to false if you do not want to consider value i anymore 
    var restart = true // Restart the loop each time a domain is changed...
    while (outcome != CPOutcome.Failure && restart) {
      restart = false

      for { i <- 0 until low.length; if (skipList(i)) } {
        // i is the index of the value to process

        val candidates = X2 filter (_.hasValue(constrainedValues(i))) toSet
        val boundCandidates = candidates filter (x => x.isBound())

        if (candidates.size < low(i)) {
          // Not enough candidates to satisfy ith cardinality constraint
          outcome = CPOutcome.Failure
        } else if (candidates.size == low(i)) {
          // Exactly enough candidates to satisfy ith cardinality constraint= 
          for (x <- candidates diff boundCandidates) {
            outcome = x.assign(constrainedValues(i))
            restart = true
            skipList(i) = false // Stop considering this value in the loop
          }
        } else if (boundCandidates.size >= low(i) && candidates.size < up(i)) {
          skipList(i) = false // Stop considering this value in the loop
        } else if (boundCandidates.size == up(i)) {
          for (x <- candidates diff boundCandidates) {
            outcome = x.removeValue(constrainedValues(i))
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
      }
    }

    // Check overall feasibility after potential changes

    // Subset of X2 such that each variable has some values out of the set of cardinality constrained values 
    val X3 = X2 filter (x => x.getMin() < constrainedValues.first || x.getMax() > constrainedValues.last)
    if (low.sum > X2.size) {
      // DEBUG
      println("Too few variables to satisfy the lower bounds of CC")
      outcome = CPOutcome.Failure
    } else if (X2.size - up.sum > X3.size) {
      // DEBUG
      println("Too many variables to satisfy the upper bounds of CC")
      outcome = CPOutcome.Failure
    }

    val candidatesByValue = constrainedValues map (v => X filter (_.hasValue(v)) toList) toArray
    // DEBUG
    //    println("something changed:" + X.mkString(","));

    outcome
  }

//  def low():Array[Int] = low
//  def up():Array[Int] = up
  
  /**
   * check a solution
   */
  def check(): Boolean = {
    check(List.tabulate(low.size)(v => v))
  }

  /**
   * check a solution
   */
  def check(indexes: List[Int]): Boolean = {
    indexes.isEmpty match {
      case false =>
        val i = indexes.head
        val histogram = X count (_.getValue() == constrainedValues(i))
        val outcome = (histogram >= low(i) && histogram <= up(i))
        // DEBUG 
        if (!outcome) { println("" + low(i) + "<?>" + histogram + "<?>" + up(i)) }
        outcome && check(indexes.tail)
      case true => true
    }
  }
  
  override def toString(): String = {
    println("Values: "+ constrainedValues.mkString(" "))
    println("Low   : "+low.mkString(" "))
    println("Up    : "+up.mkString(" "))
	""	  
  }
}


object GCCFWC {
  def main(args: Array[String]) {
//    var nbSol = 0  
//    import scampi.cp.test.TestGCCFWC._
//    val cp = new CPSolver()
//    
//    // Generate the constraint and solve
//    val GCC = GCCGen(cp)
//    cp.solveAll subjectTo {
//      cp.add(GCC)
//    } exploration {
//      cp.binaryFirstFail(GCC.getX)
//      println(GCC)
//      println((GCC.getX map (_.getValue())).mkString(";"))
//      GCC.check()
//      nbSol += 1
//    }
    
  }
}
