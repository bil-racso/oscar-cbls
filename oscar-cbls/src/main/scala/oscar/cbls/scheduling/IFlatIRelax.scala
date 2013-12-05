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
package oscar.cbls.scheduling

/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

import oscar.cbls.scheduling.algo.ConflictSearch
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.{IntVar, Solution, Model}

class IFlatIRelax(p: Planning, Verbose: Boolean = true) extends SearchEngine {
  val model: Model = p.model

  class FlatteningHeuristics()
  case class EarliestFirst() extends FlatteningHeuristics
  case class WorseFirst() extends FlatteningHeuristics
  case class Random() extends FlatteningHeuristics

  /**This solves the jobshop by iterative relaxation and flattening
    * @param MaxIt the max number of iterations of the search
    * @param Stable the number of no successice noimprove that will cause the search to stop
    */
  def Solve(MaxIt: Int,
            Stable: Int,
            flatteningheursitics: FlatteningHeuristics = WorseFirst(),
            NbRelax: Int = 4,
            PkillPerRelax: Int = 50) {

    var it: Int = 0

    flatteningheursitics match {
      case EarliestFirst() => FlattenEarliestFirst();
      case WorseFirst() => FlattenWorseFirst();
      case Random() => RandomFlatten();
    }

    var BestSolution: Solution = model.getSolution(true)
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

      if(plateaulength > 10 && (plateaulength % 50) == 0){

        for (i <- 0 until NbRelax*3){Relax(PkillPerRelax);}
        println("jumping****************")

      }else{
        val m = p.MakeSpan.value
        RelaxUntilMakespanReduced(PkillPerRelax,NbRelax)
//        for (i <- 0 until NbRelax) {
//          Relax(PkillPerRelax);
//        }
        if(p.MakeSpan.value == m)println("skip")
      }

      flatteningheursitics match {
        case EarliestFirst() => FlattenEarliestFirst();
        case WorseFirst() => FlattenWorseFirst();
        case Random() => RandomFlatten();
      }

      println(p.MakeSpan)
      println("iteration: " + it)

      if (p.MakeSpan.value < BestMakeSpan) {
        BestSolution = model.getSolution(true)
        BestMakeSpan = p.MakeSpan.value
        plateaulength = 0
        println("Better MakeSpan found")
        p.updateVisual()
      } else {
        plateaulength += 1
      }

      println("----------------")
    }
    model.restoreSolution(BestSolution)

    println("restored best solution")

    p.updateVisual()
  }

  /**
   * performs the relaxation of the critical path
   * @param PKill: the probability to kill a killable precedence constraint in percent
   */
  def Relax(PKill: Int) {

    val PotentiallykilledNodes = CriticalPathFinder.nonSolidCriticalPath(p)
    if (PotentiallykilledNodes.isEmpty) return

    //val (from, to) = selectFrom(PotentiallykilledNodes)
    //if (Verbose) println("killed " + from + "->" + to)
    //to.removeDynamicPredecessor(from)

    for ((from,to) <- PotentiallykilledNodes){
      if (flip(PKill)){
        if (Verbose) println("killed " + from + "->" + to)
        to.removeDynamicPredecessor(from)
      }
    }
  }

  /**
   * performs the relaxation of the critical path
   * @param PKill: the probability to kill a killable precedence constraint in percent
   * @param min: the minimal number of relaxation
   */
  def RelaxUntilMakespanReduced(PKill:Int, min:Int = 3){
    val m = p.MakeSpan.value
    var n = 0;
    while((p.MakeSpan.value == m) | (n < min)){
      n +=1
      Relax(PKill)
    }
    println("relaxed " + n + " times to shorten makespan")

  }

  def RandomFlatten() {
    while (!p.EarliestOvershotResources.value.isEmpty) {
      val r: CumulativeResource = p.ResourceArray(selectFrom(p.EarliestOvershotResources.value))
      val t: Int = r.FirstOvershoot.value

      val ActivitiesAndUse = r.ActivitiesAndUse.filter((taksAndamount: (Activity, IntVar)) => r.Use(t).value.contains(taksAndamount._1.ID))
      val Activities: List[Activity] = ActivitiesAndUse.map((ActivityAndamount: (Activity, IntVar)) => ActivityAndamount._1)

      val a = selectFrom(Activities)
      val b = selectFrom(Activities, (j: Activity) => j != a)

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }

  /**implements the standard flatten procedure*/
  def FlattenWorseFirst() {
    while (!p.WorseOvershotResource.value.isEmpty) {
      val r: CumulativeResource = p.ResourceArray(selectFrom(p.WorseOvershotResource.value))
      val t: Int = selectFirst(r.HighestUsePositions.value)

      val ActivitiesAndUse = r.getActivitiesAndUse(t)

      val conflictSet: List[(Activity, IntVar)] = ConflictSearch(
        0,
        ActivitiesAndUse,
        (use: Int, ActivityAndamount: (Activity, IntVar)) => use + ActivityAndamount._2.value,
        (use: Int, ActivityAndamount: (Activity, IntVar)) => use - ActivityAndamount._2.value,
        (use: Int) => use > r.MaxAmount
      )

      val conflictActivities: List[Activity] = conflictSet.map(_._1)

      //TODO: it could be the case tat no pair of Activity is available here.
      val (a, b) = selectMax2(conflictActivities, conflictActivities,
        (a: Activity, b: Activity) => (b.LatestEndDate.value - a.EarliestStartDate.value),
        (a: Activity, b: Activity) => p.canAddPrecedenceAssumingResourceConflict(a,b))

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }

  def FlattenEarliestFirst() {
    while (!p.EarliestOvershotResources.value.isEmpty) {
      val r: CumulativeResource = p.ResourceArray(selectFrom(p.EarliestOvershotResources.value))
      val t: Int = r.FirstOvershoot.value

      //the two selected Activities a,b must belong to a minimal conflict set
      //and they must maximize lsd(b)-esd(a)  //pq pas led(b) - esd(a)??
      //then insert a->b

      //modification par rapport a l'heuristique:
      //uniquement b doit appartenir au conflict set.
      //et on maximise led(b) - esd(a)

      val ActivitiessAndUse = r.getActivitiesAndUse(t)

      val conflictSet: List[(Activity, IntVar)] = ConflictSearch(
        0,
        ActivitiessAndUse,
        (use: Int, ActivityAndamount: (Activity, IntVar)) => use + ActivityAndamount._2.value,
        (use: Int, ActivityAndamount: (Activity, IntVar)) => use - ActivityAndamount._2.value,
        (use: Int) => use > r.MaxAmount
      )

      val conflictActivities: List[Activity] = conflictSet.map(_._1)

      val Activities: List[Activity] = ActivitiessAndUse.map(_._1)

      val (a, b) = selectMax2(Activities, conflictActivities,
        (a: Activity, b: Activity) => (b.LatestStartDate.value - a.EarliestEndDate.value),
        (a: Activity, b: Activity) => a != b)

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }
}


