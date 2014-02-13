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
package oscar.cbls.scheduling.algo

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.{CBLSIntVar, Solution, Store}
import oscar.cbls.scheduling.model._
import oscar.cbls.invariants.core.computation.Solution
import oscar.cbls.scheduling.model.CumulativeResource

/**
 * @param p
 * @param Verbose
 * @author renaud.delandtsheer@cetic.be
 * */
class IFlatIRelaxTabu(p: Planning, Verbose: Boolean = true) extends SearchEngine {
  val model: Store = p.model


  /**This solves the jobshop by iterative relaxation and flattening
    * @param MaxIt the max number of iterations of the search
    * @param Stable the number of no successice noimprove that will cause the search to stop
    */
  def Solve(MaxIt: Int,
            Stable: Int,
            NbRelax: Int = 4,
            PkillPerRelax: Int = 50,
            tenure:Int = 3,
            jumpAfterPlateau:Int = 5) {


    //the iteration where the precedence from-to has been killed last
    val tabu:Array[Array[Int]] = Array.fill(p.activityCount,p.activityCount)(initialTabu)

    var it: Int = 0

    FlattenWorseFirst(it, tabu)

    var BestSolution: Solution = model.solution(true)
    if (Verbose) {
      println(p.MakeSpan)
      println("----------------")
    }
    p.updateVisual()

    var plateaulength = 0
    var BestMakeSpan = p.MakeSpan.value

    while (it < MaxIt && plateaulength < Stable) {
      //iterative weakening and flattening
      it += 1

      if((plateaulength % jumpAfterPlateau) == 0){

        for (i <- 0 until NbRelax*3){Relax(PkillPerRelax, it, tabu, tenure);}
        println("jumping****************")

      }else{
        val m = p.MakeSpan.value
        if (!RelaxUntilMakespanReduced(PkillPerRelax,NbRelax, it, tabu, tenure)) return
        if(p.MakeSpan.value == m)println("skip")
      }

      FlattenWorseFirst(it, tabu)

      println(p.MakeSpan)
      println("iteration: " + it)

      if (p.MakeSpan.value < BestMakeSpan) {
        BestSolution = model.solution(true)
        BestMakeSpan = p.MakeSpan.value
        plateaulength = 0
        println("Better MakeSpan found")
        p.updateVisual()
      } else {
        plateaulength += 1
        p.clean()
      }

      println("----------------")
    }
    model.restoreSolution(BestSolution)

    p.clean()

    println("restored best solution")

    p.updateVisual()
  }

  /**
   * performs the relaxation of the critical path
   * @param PKill: the probability to kill a killable precedence constraint in percent
   * @return true if something could be relaxed, false if makespan is solid (made only of dependencies that cannot be relaxed)
   */
  def Relax(PKill: Int, it:Int, tabu:Array[Array[Int]], tenure:Int):Boolean = {

    val PotentiallykilledNodes = CriticalPathFinder.nonSolidCriticalPath(p)
    if (PotentiallykilledNodes.isEmpty) return false

    for ((from,to) <- PotentiallykilledNodes){
      if (flip(PKill)){
        to.removeDynamicPredecessor(from,Verbose)
        setTabu(from, to, it, tabu, tenure)
      }
    }
    true
  }

  /**
   * performs the relaxation of the critical path
   * @param PKill: the probability to kill a killable precedence constraint in percent
   * @param min: the minimal number of relaxation
   * @return true if something could be relaxed, false if makespan is solid
   */
  def RelaxUntilMakespanReduced(PKill:Int, min:Int = 3, it:Int, tabu:Array[Array[Int]], tenure:Int):Boolean = {
    val m = p.MakeSpan.value
    var n = 0
    var SomethingCouldBeRelaxed = false
    while((p.MakeSpan.value == m) | (n < min)){
      n +=1
      SomethingCouldBeRelaxed = SomethingCouldBeRelaxed | Relax(PKill, it, tabu, tenure)
      if (!SomethingCouldBeRelaxed) return false
    }
    if (Verbose) println("relaxed " + n + " times to shorten makespan")
    return SomethingCouldBeRelaxed
  }

  /**implements the standard flatten procedure*/
  def FlattenWorseFirst(it:Int, tabu:Array[Array[Int]]) {
    while (!p.WorseOvershotResource.value.isEmpty) {
      val r: Resource = p.ResourceArray(selectFrom(p.WorseOvershotResource.value))

      val t: Int = r.worseOverShootTime

      val conflictActivities=r.conflictingActivities(t)

      selectMax2(conflictActivities, conflictActivities,
        (a: Activity, b: Activity) => (b.LatestEndDate.value - a.EarliestStartDate.value)/getWorseningFactor(a, b, it, tabu),
        (a: Activity, b: Activity) => p.canAddPrecedenceAssumingResourceConflict(a,b))
      match{
        case (a,b) =>
          b.addDynamicPredecessor(a,Verbose)
        case null =>

          //no precedence can be added because some additional precedence must be killed to allow that
          //this happens when superTasks are used, and when dependencies have been added around the start and end tasks of a superTask
          //we search which dependency can be killed in the conflict set,
          val conflictActivityArray = conflictActivities.toArray
          val dependencyKillers:Array[Array[PrecedenceCleaner]] =
            Array.tabulate(conflictActivityArray.size)(
              t1 => Array.tabulate(conflictActivityArray.size)(
                t2 => p.getDependencyToKillToAvoidCycle(conflictActivityArray(t1),conflictActivityArray(t2))))

          val (a,b) = selectMax2(conflictActivityArray.indices, conflictActivityArray.indices,
            (a: Int, b: Int) => (conflictActivityArray(b).LatestEndDate.value - conflictActivityArray(a).EarliestStartDate.value)/getWorseningFactor(conflictActivityArray(a), conflictActivityArray(b), it, tabu),
            (a: Int, b: Int) => dependencyKillers(a)(b).canBeKilled)

          println("need to kill dependencies to complete flattening")
          dependencyKillers(a)(b).killDependencies(Verbose)

          conflictActivityArray(b).addDynamicPredecessor(conflictActivityArray(a),Verbose)
      }
    }
  }

  /**you should divide the metric to maximize when inserting a dependency by this worseningFactor*/
  def getWorseningFactor(from:Activity, to:Activity, it:Int, tabu:Array[Array[Int]]):Int = {
    val thisTabu = tabu(from.ID)(to.ID)
    val toreturn = scala.math.max(1,thisTabu-it)
    //println("worseningFactor: " + toreturn)
    toreturn
  }

  def setTabu(from:Activity, to:Activity, it:Int, tabu:Array[Array[Int]], tenure:Int){
    tabu(from.ID)(to.ID) = scala.math.max(it,tabu(from.ID)(to.ID)) + tenure
  }

  val initialTabu = 0

}


