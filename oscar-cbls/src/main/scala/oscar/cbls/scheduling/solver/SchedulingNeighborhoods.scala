package oscar.cbls.scheduling.solver

import oscar.cbls.scheduling.algo.CriticalPathFinder
import oscar.cbls.scheduling.model.{Activity, Planning, PrecedenceCleaner, Resource}
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.core._

//TODO: attention: il ne faut protégeer l'objectif que sur les schedules acceptables.
//donc il faut une fct objectif acceptable qui vale MaxInt si le schedule ne l'est pas.

/**
 * @param p
 * @param maxIterations
 * @param estimateMakespanExpansionForNewDependency  This computes an estimate of the MakeSpan expansion if the given precedence is added.
 *                                                   this estimate is completely wrong in itself, as a constant factor is added to each estimate.
 *                                                   since it is the same factor, you can use this method to chose among a set of precedence
 *                                                   because this will forget about the correcting factor.
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 * */
case class FlattenWorseFirst(p:Planning, maxIterations:Int,
                             estimateMakespanExpansionForNewDependency:(Activity,Activity) => Int = (from: Activity, to: Activity) => from.earliestEndDate.value - to.latestStartDate.value,
                             supportForSuperTasks:Boolean = false)
  extends JumpNeighborhood with SearchEngineTrait{

  override def shortDescription(): String = "Flattening worse first"

  //this resets the internal state of the Neighborhood
  override def reset(){}

  /**implements the standard flatten procedure*/
  override def doIt() {
    var iterations = 0
    while (!p.worseOvershotResource.value.isEmpty) {
      if (iterations > maxIterations)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. " +
          "Check there is no conflict between non movable activities.")
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

          if(!supportForSuperTasks)
            throw new Error("cannot flatten until conflict resolution, maybe your model has superTasks?" +
              " if yes set supportForSuperTasks, otherwise, problem with non-movable activities")

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
 * @param p the planning to relax
 * @param pKill the probability to kill a killable precedence constraint in percent. must be bigger than 10 (otherwise this will crash
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 * */
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

/**
 * @param p the planning to relax
 * relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource
 * Warning: can only be called if there are no existing conflict!!
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 * */
case class RelaxNoConflict(p:Planning) extends JumpNeighborhood with SearchEngineTrait {

  override def doIt{
    require(p.worseOvershotResource.value.isEmpty)
    for (t: Activity <- p.activityArray) {
      for (iD: Int <- t.additionalPredecessors.value) {
        t.removeDynamicPredecessor(p.activityArray(iD), false)
        if(!p.worseOvershotResource.value.isEmpty)
          t.addDynamicPredecessor(p.activityArray(iD), false)
      }
    }
  }

  override def shortDescription(): String = "relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource"

  //this resets the internal state of the Neighborhood
  override def reset(){}
}


/**removes all additional Activity precedences that are not tight
  * @param p the planning to relax
  * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
  * */
case class CleanPrecedences(p:Planning) extends JumpNeighborhood with SearchEngineTrait {

  override def doIt{
    for (t: Activity <- p.activityArray) {
      for (iD: Int <- t.additionalPredecessors.value) {
        if (!t.potentiallyKilledPredecessors.value.contains(iD)) {
          t.removeDynamicPredecessor(p.activityArray(iD), amIVerbose)
        }
      }
    }
  }

  override def shortDescription(): String = "removes all additional Activity precedences that are not tight"

  //this resets the internal state of the Neighborhood
  override def reset(){}
}



/**
 * @param p
 * @param verbose
 * @author renaud.delandtsheer@cetic.be
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 */
class IFlatIRelax2(p: Planning,
                   verbose: Boolean = true,
                   nbRelax: Int = 4,
                   pkillPerRelax: Int = 50) {

  require(p.model.isClosed, "model should be closed before iFlatRelax algo can be instantiated")
  val maxIterationsForFlatten = (p.activityCount * (p.activityCount - 1)) / 2

  /**
   * This solves the jobshop by iterative relaxation and flattening
   * @param maxIt the max number of iterations of the search
   * @param stable the number of no successive noimprove that will cause the search to stop
   */
  def solve(maxIt: Int, stable: Int){

    val saver = (NoMoveNeighborhood() protectBest p.makeSpan)
    val iFLatRelaxStrategy = (FlattenWorseFirst(p,maxIterationsForFlatten) maxMoves 1 exhaustBack saver exhaustBack
      (Relax(p, pkillPerRelax) untilImprovement(p.makeSpan, 3, maxIterationsForFlatten))
      maxMoves stable withoutImprovementOver p.makeSpan )

    iFLatRelaxStrategy.doAllMoves(_ >= maxIt)

    saver.restoreBest()
  }
}

