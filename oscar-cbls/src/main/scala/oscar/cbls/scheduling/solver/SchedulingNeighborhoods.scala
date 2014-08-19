package oscar.cbls.scheduling.solver

import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.scheduling.algo.CriticalPathFinder
import oscar.cbls.scheduling.model.{Activity, PrecedenceCleaner, Resource, Planning}
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.combinators.DoOnMove
import oscar.cbls.search.core.{SearchResult, Neighborhood, JumpNeighborhoodParam, JumpNeighborhood}
import oscar.cbls.search.move.Move


//TODO: attention: il ne faut protégeer l'objectif que sur les schedules acceptables.
  //donc il faut une fct objectif acceptable qui vale MaxInt si le schedule ne l'est pas.

/**
 *
 * @param p
 * @param maxIterations
 * @param estimateMakespanExpansionForNewDependency  This computes an estimate of the Makespan expansion if the given precedence is added.
 *                                                   this estimate is completely wrong in itself, as a constant factor is added to each estimate.
 *                                                   since it is the same factor, you can use this method to chose among a set of precedence
 *                                                   because this will forget about the correcting factor.
 */
case class FlattenWorseFirst(p:Planning, maxIterations:Int,
                             estimateMakespanExpansionForNewDependency:(Activity,Activity) => Int = (from: Activity, to: Activity) => from.earliestEndDate.value - to.latestStartDate.value)
  extends JumpNeighborhood with SearchEngineTrait{

  override def shortDescription(): String = "Flattening worse first"

  //this resets the internal state of the Neighborhood
  override def reset(){}

  /**implements the standard flatten procedure*/
  override def doIt() {
    var iterations = 0
    while (!p.worseOvershotResource.value.isEmpty) {
      if (iterations > maxIterations)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. Check there is no conflict between non moveable activities.")
      iterations += 1

      // the most violated resource
      val r: Resource = p.resourceArray(selectFrom(p.worseOvershotResource.value))

      // the first violation of the resource in time
      val t: Int = r.worseOverShootTime

      val conflictActivities = r.conflictingActivities(t)
      val baseForEjection = r.baseActivityForEjection(t)

      selectMin2(baseForEjection, conflictActivities,
        estimateMakespanExpansionForNewDependency,
        p.canAddPrecedenceAssumingResourceConflict) match {
        case (a, b) =>
          b.addDynamicPredecessor(a, amIVerbose)
        case null =>

          //no precedence can be added because some additional precedence must be killed to allow that
          //this happens when superTasks are used, and when dependencies have been added around the start and end tasks of a superTask
          //we search which dependency can be killed in the conflict set,
          val conflictActivityArray = conflictActivities.toArray
          val baseForEjectionArray = baseForEjection.toArray

          val dependencyKillers: Array[Array[PrecedenceCleaner]] =
            Array.tabulate(baseForEjection.size)(
              t1 => Array.tabulate(conflictActivityArray.size)(
                t2 => p.getDependencyToKillToAvoidCycle(baseForEjectionArray(t1), conflictActivityArray(t2))))

          selectMin2(baseForEjectionArray.indices, conflictActivityArray.indices,
            (a: Int, b: Int) => estimateMakespanExpansionForNewDependency(baseForEjectionArray(a), conflictActivityArray(b)),
            (a: Int, b: Int) => dependencyKillers(a)(b).canBeKilled) match {
            case (a, b) => {
              if (amIVerbose) println("need to kill dependencies to complete flattening")
              dependencyKillers(a)(b).killDependencies(amIVerbose)

              conflictActivityArray(b).addDynamicPredecessor(baseForEjectionArray(a), amIVerbose)
            }
            case null => throw new Error("cannot flatten at time " + t + " activities: " + conflictActivities)
          }

      }
    }
  }
}

/**
 * @param p
 * @param pKill the probability to kill a killable precedence constraint in percent. must be bigger than 10 (otherwise this will crash
 */
case class Relax(p:Planning, pKill: Int,
                 doRelax:(Activity, Activity, Boolean) => Unit = (from: Activity, to: Activity, verbose:Boolean) => to.removeDynamicPredecessor(from, verbose))
  extends JumpNeighborhoodParam[List[(Activity, Activity)]] with SearchEngineTrait {

  override def doIt(potentiallyKilledPrecedences: List[(Activity, Activity)]){
    for ((from, to) <- potentiallyKilledPrecedences) {
        doRelax(from, to, amIVerbose)
    }
  }

  override def getParam(): List[(Activity, Activity)] = {
    val potentiallyKilledPrecedences = CriticalPathFinder.nonSolidCriticalPath(p)()
    if (potentiallyKilledPrecedences.isEmpty) null
    else{
      var toReturn:List[(Activity, Activity)] = List.empty
      var maxTrials = 0
      while(toReturn.isEmpty && maxTrials < 10) {
        maxTrials += 1
        toReturn = potentiallyKilledPrecedences.filter(_ => flip(pKill))
      }
      if (toReturn.isEmpty) potentiallyKilledPrecedences
      else toReturn
    }
  }

  override def getShortDescription(param: List[(Activity, Activity)]): String = "Relax critical Path " + param

  //this resets the internal state of the Neighborhood
  override def reset(){}
}


case class RelaxUntilMakespanReduced(p:Planning, maxIterations:Int, pKill: Int,
                                     doRelax:(Activity,Activity, Boolean) => Unit = (from: Activity, to: Activity, verbose:Boolean) => to.removeDynamicPredecessor(from, verbose),
                                     minRelax: Int = 3) extends Neighborhood {
  private var oldMakespan: Int = 0
  private var makeSpanReduced: Boolean = false

  private val baseNeighborhood = DoOnMove(Relax(p, pKill, doRelax) maxMoves (minRelax - 1) orElse (Relax(p, pKill, doRelax) when (() => !makeSpanReduced)),
    proc = () => {oldMakespan = p.makeSpan.value},
    procAfterMove = () => {if (p.makeSpan.value < oldMakespan) makeSpanReduced = true})

  override def getImprovingMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = baseNeighborhood.getImprovingMove(acceptanceCriterion)

  override def reset(): Unit = {
    baseNeighborhood.reset
    makeSpanReduced =  false
  }
}
