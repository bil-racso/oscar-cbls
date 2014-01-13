package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation.{IntVar, SetVar, Store}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.search.{StopWatch, SearchEngineTrait}
import oscar.cbls.modeling._
import oscar.cbls.constraints.core.{ConstraintSystem, Constraint}
import oscar.cbls.objective.ObjectiveTrait
import scala.collection.immutable.SortedSet


/** this is a helper object that you can extend to implement your solver with the minimal syntactic overhead.
  * It imports all the methods that you might need to develop your own solver based on the CBLS approach
  *
  * @param verbose requires that the propagation structure prints a trace of what it is doing. all prints are preceded by ''PropagationStruture''
  * @param checker specifies that once propagation is finished, it must call the checkInternals method on all propagation elements.
  * @param noCycle is to be set to true only if the static dependency graph between propagation elements has no cycles. If unsure, set to false, the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
  * @param topologicalSort set to true if you want to use topological sort, to false for layered sort (layered is faster)
  * @param propagateOnToString set to true if a toString triggers a propagation, to false otherwise. Set to false only for deep debugging
  *
  **/
class CBLSSolver(override val verbose:Boolean = false,
                 override val checker:Option[Checker] = None,
                 override val noCycle:Boolean = true,
                 override val topologicalSort:Boolean = false,
                 override val propagateOnToString:Boolean = true)
  extends Store(verbose, checker, noCycle, topologicalSort,propagateOnToString)
  with SearchEngineTrait
  with AlgebraTrait
  with Constraints
  with ClusterInvariants
  with ComplexLogicInvariants
  with AccessInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with StopWatch{

  val c = new ConstraintSystem(this)


}
