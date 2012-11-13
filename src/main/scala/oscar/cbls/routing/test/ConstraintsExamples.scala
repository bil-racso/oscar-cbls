import math._
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.routing._

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 12/11/12
 * Time: 14:22
 * To change this template use File | Settings | File Templates.
 */

object ConstraintsExamples {

  val N = 500
  val vehicles = 2
  val kLimited = 20

  val m: Model = new Model(false,false,false,false)
  val vrp = new VRP(N, vehicles, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
    with PenaltyForUnrouted with OtherFunctionToObjective with WeakConstraints with StrongConstraints

  def distanceMatrix(towns : Array[Point]):Array[Array[Int]] =
    Array.tabulate(N,N)((i,j) => round(sqrt((pow(towns(i).long - towns(j).long, 2)
      + pow(towns(i).lat - towns(j).lat, 2) ).toFloat)).toInt )

  vrp.installCostMatrix(distanceMatrix(InstanceVRP.random(N)))
  vrp.saveKNearestPoints(kLimited)

  val strongConstraintSystem = new ConstraintSystem(m)
  val weakConstraintSystem = new ConstraintSystem(m)

  /*
    EXAMPLE 1:
    ----------
    Example of constraints based on unrouted node.

   */
  // fix node penalty
  vrp.fixUnroutedPenaltyWeight(100)
  //vrp.fixUnroutedPenaltyWeight(2,222) //specific weight for a given node
  val nbOfAllowedUnroutedNode = new IntVar(m,0,N,10,"nb of allowed unrouted node")
  // post and register the constraint
  weakConstraintSystem.post(LE(Cardinality(vrp.Unrouted),nbOfAllowedUnroutedNode),vrp.UnroutedPenalty)
  weakConstraintSystem.registerForViolation(vrp.Unrouted)
  // could be also strongConstraint if needed.

  /*
  * Or simply add the weight of an unrouted node to the objective.
  */

  vrp.recordAddedFunction(vrp.UnroutedPenalty)


  /*
    EXAMPLE 2:
    ----------
    Example of constraints based on route length. (max length in this example, easy to adapt for a min length too)

   */
  // fix a penalty (use node weight, or a penalty fixed by route or for all route, or anything else) and a length max.
  val penalty = new IntVar(m,0,N,1000,"penalty for too long route")
  val lengthMax = new IntVar(m,0,N,50,"length max for route")
  // post and register the constraint
  for(i <- 0 until vrp.V){
    strongConstraintSystem.post(LE(vrp.RouteLength(i),lengthMax),penalty)
    strongConstraintSystem.registerForViolation(vrp.RouteLength(i))
    // once more it could be a weakConstraint, it depends only of the problem's definition.
  }

  /*
    EXAMPLE 3:
    ----------
    Example of constraints based on capacity of vehicle. (CVRP)

   */








  vrp.setStrongConstraints(strongConstraintSystem)
  vrp.setWeakConstraints(weakConstraintSystem)
  strongConstraintSystem.close()
  weakConstraintSystem.close()
  m.close()
}