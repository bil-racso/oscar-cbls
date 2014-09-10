package oscar.cbls.scheduling.solver

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.scheduling.algo.CriticalPathFinder
import oscar.cbls.scheduling.model._
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.core._

/**
 * @param p the planning to flatten
 * @param maxIterations the max number of the flattening.
 *                      This is a safety parameter to prevent infinite loop,
 *                      you can set it to (p.activityCount * (p.activityCount - 1)) / 2
 * @param estimateMakespanExpansionForNewDependency  This computes an estimate of the MakeSpan expansion if the given precedence is added.
 *                                                   this estimate is completely wrong in itself, as a constant factor is added to each estimate.
 *                                                   since it is the same factor, you can use this method to chose among a set of precedence
 *                                                   because this will forget about the correcting factor.
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 * */
case class FlattenWorseFirst(p:Planning,
                             maxIterations:Int,
                             estimateMakespanExpansionForNewDependency:(Activity,Activity) => Int =
                             (from: Activity, to: Activity) => from.earliestEndDate.value - to.latestStartDate.value,
                             supportForSuperTasks:Boolean = false, priorityToPrecedenceToMovableActivities:Boolean = true)
  extends JumpNeighborhood with SearchEngineTrait {

  override def shortDescription(): String = "Flattening worse first"

  //this resets the internal state of the Neighborhood
  override def reset() {}

  /** implements the standard flatten procedure */
  override def doIt() {
    var iterations = 0
    while (p.worseOvershotResource.value.nonEmpty) {
      if (iterations > maxIterations)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. " +
          "Check there is no conflict between non movable activities.")
      iterations += 1

      // the most violated resource
      val r: Resource = p.resourceArray(selectFrom(p.worseOvershotResource.value))

      // the worse violation of the resource in time
      val t: Int = r.worseOverShootTime

      flattenOne(r, t)
    }
  }

  def flattenOne(r: Resource, t: Int): Unit = {
    val conflictActivities = r.conflictingActivities(t)
    val baseForEjection = r.baseActivityForEjection(t)

    val makeSpanExpansionEstimator = (if (priorityToPrecedenceToMovableActivities)
      (from: Activity, to: Activity) =>
        2 * estimateMakespanExpansionForNewDependency(from, to)
          +(if (from.isInstanceOf[NonMoveableActivity]) 1 else 0)
    else estimateMakespanExpansionForNewDependency)

    selectMin2[Activity, Activity](baseForEjection, conflictActivities,
      makeSpanExpansionEstimator,
      p.canAddPrecedenceAssumingResourceConflict)
    match {
      case (a, b) =>
        b.addDynamicPredecessor(a, amIVerbose)
        return
      case null => ;
    }

    if (!supportForSuperTasks)
      throw new Error("cannot flatten until conflict resolution, maybe your model has superTasks?" +
        " if yes set supportForSuperTasks, otherwise, problem with non-movable activities")

    flattenOneWithSuperTaskHandling(r, t)
  }

  def flattenOneWithSuperTaskHandling(r: Resource, t: Int): Unit = {
    val conflictActivities = r.conflictingActivities(t)
    val baseForEjection = r.baseActivityForEjection(t)

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
      case (a, b) =>
        if (amIVerbose) println("need to kill dependencies to complete flattening")
        dependencyKillers(a)(b).killDependencies(amIVerbose)

        conflictActivityArray(b).addDynamicPredecessor(baseForEjectionArray(a), amIVerbose)

      case null => throw new Error("cannot flatten at time " + t + " activities: " + conflictActivities)
    }
  }
}

/**
 * @param p the planning to relax
 * @param pKill the probability to kill a killable precedence constraint in percent. must be bigger than 10 (otherwise this will crash
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 * */
case class Relax(p:Planning, pKill: Int,
                 doRelax:(Activity, Activity, Boolean) => Unit = (from: Activity, to: Activity, verbose:Boolean) => to.removeDynamicPredecessor(from, verbose))
//                (activitiestoRelax:()=>Iterable[Int] = p.sentinelActivity.staticPredecessorsID) TODO: add the possibility to search from given terminating tasks
  extends JumpNeighborhoodParam[List[(Activity, Activity)]] with SearchEngineTrait {

  override def doIt(potentiallyKilledPrecedences: List[(Activity, Activity)]){
    for ((from, to) <- potentiallyKilledPrecedences) {
      doRelax(from, to, amIVerbose)
    }
  }

  override def getParam: List[(Activity, Activity)] = {
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

/**
 * relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource
 * Warning: can only be called if there are no existing conflict!!
 * @param p the planning to relax
 * @param twoPhaseCheck set to true for a possibly faster move evaluation,
 *                      but this depends on your model and propagation setup,
 *                      so you need to experiment on this option.
 *                      it has no influence on the result, only on the speed of this neighborhood.
 */
case class RelaxNoConflict(p:Planning, twoPhaseCheck:Boolean = false) extends JumpNeighborhood with SearchEngineTrait {

  override def doIt(): Unit ={
    var relaxCount = 0
    var improved = true
    while (improved) {
      improved = false

      for (t: Activity <- p.activityArray) {
        for (iD: Int <- t.additionalPredecessors.value) {
          val testedPredecessor = p.activityArray(iD)
          val wasCriticalDependency = if (twoPhaseCheck) t.potentiallyKilledPredecessors.value.contains(iD) else true
          t.removeDynamicPredecessor(testedPredecessor, false)
          if (!wasCriticalDependency || p.worseOvershotResource.value.isEmpty) {
            relaxCount += 1
            improved = true
          }else{
            t.addDynamicPredecessor(testedPredecessor, false)
          }
        }
      }
    }
    if(amIVerbose) println("RelaxNoConflict: relaxCount:" + relaxCount)
  }
  override def shortDescription(): String = "relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource)"
}

/**removes all additional Activity precedences that are not tight
  * @param p the planning to relax
  * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
  * */
case class CleanPrecedences(p:Planning) extends JumpNeighborhood with SearchEngineTrait {

  override def doIt(){
    for (t: Activity <- p.activityArray) {
      for (iD: Int <- t.additionalPredecessors.value) {
        if (!t.potentiallyKilledPredecessors.value.contains(iD)) {
          t.removeDynamicPredecessor(p.activityArray(iD), amIVerbose)
        }
      }
    }
  }

  override def shortDescription(): String = "removes all additional Activity precedences that are not tight"
}

object SchedulingStrategies{

  /**
   * @param p the planning
   * @param nbRelax the minimal number of relax to perform (actually, we relax until makespan reduced, with an upper bound just in case
   * @param pKillPerRelax the probability of killing a precedence for each precedence on the critical path considered during a relax
   * @param stable the number of no successive no improve that will cause the search to stop
   * @param objective: the objective, typically the makespan, but you could try something else
   * @return a neighborhood, you just have to do all moves, and restore the best solution
   */
  def iFlatRelax(p: Planning,
                 nbRelax: Int = 4,
                 pKillPerRelax: Int = 50,
                 stable: Int,
                 objective:CBLSIntVar):Neighborhood = {
    require(p.model.isClosed, "model should be closed before iFlatRelax algo can be instantiated")
    val maxIterationsForFlatten = (p.activityCount * (p.activityCount - 1)) / 2

    val searchLoop = FlattenWorseFirst(p,maxIterationsForFlatten) maxMoves 1 exhaustBack
      Relax(p, pKillPerRelax) untilImprovement(p.makeSpan, nbRelax, maxIterationsForFlatten)

    (searchLoop maxMoves stable withoutImprovementOver objective
      protectBest objective whenEmpty p.worseOvershotResource)
  }
}
