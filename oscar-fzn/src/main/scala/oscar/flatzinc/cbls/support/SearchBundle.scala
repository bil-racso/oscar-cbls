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
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls.support

import scala.util.Random
import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.invariants.core.computation.CBLSSetVar
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.computation.SetInvariant.toIntSetVar
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.objective.{Objective => CBLSObjective}
import oscar.cbls.search.SearchEngine
import oscar.flatzinc.cbls.Log
import oscar.flatzinc.model.Domain
import oscar.flatzinc.model.DomainRange
import oscar.flatzinc.model.DomainSet
import oscar.flatzinc.cbls.FZCBLSModel


class CBLSIntVarDom(model: Store, val dom: Domain, private var Value: Int, n: String = null)
  extends CBLSIntVar(model, dom.min to dom.max, Value, n) {
  def getDomain():Iterable[Int] = {
    dom match {
      case DomainRange(min, max) => min to max
      case DomainSet(values) => values
    }
  }
  def getRandValue():Int = {
    dom match {
      case DomainRange(min, max) => (min to max)(Random.nextInt(max-min+1))
      case DomainSet(values) => values.toIndexedSeq(Random.nextInt(values.size))
    }
  }
  def domainSize = dom match {
    case DomainRange(min, max) => math.max(max-min+1,0)
    case DomainSet(values) => values.size
  }
}
case class CBLSIntConstDom(ConstValue:Int, override val model:Store = null)
  extends CBLSIntVarDom(model, new DomainRange(ConstValue,ConstValue), ConstValue, "IntConst("+ ConstValue + ")"){
  override def getValue(NewValue:Boolean=false):Int = ConstValue //pour pas avoir de propagation
  override def toString:String = "IntConst("+ ConstValue + ")"
}

object CBLSIntVarDom {
  def apply(model: Store, domain: Domain, value: Int, name: String) = {
    new CBLSIntVarDom(model, domain, value, name)
  }
}

abstract class SearchProcedure extends SearchEngine {
  
  def run(): List[(Long, Int, Int, String)]
  
  
  def showViolatedConstraints(c: ConstraintSystem){
    for(cc <- c.violatedConstraints){
      println(cc + " "+cc.violation.value)
    }
  }
}
class Chain(val a: SearchProcedure, val b: SearchProcedure) extends SearchProcedure {
  def run(): List[(Long, Int, Int, String)] = {
    val sols = a.run()
    b.run() ++ sols
  }
}

class FakeSearch extends SearchProcedure {
  def run() = {List.empty[(Long, Int, Int, String)]}
}

class SimpleLocalSearch(val m:FZCBLSModel,val objective: CBLSObjective, val MaxTimeMilli: Int) extends SearchProcedure {
  val violation: Array[CBLSIntVar] = m.vars.map(m.c.violation(_)).toArray;
  val log = m.log
  def run(): List[(Long, Int, Int, String)]={
    var solutionList: List[(Long, Int, Int, String)] = List.empty[(Long, Int, Int, String)]
    var improving = 3;
    var lastImproved = 0;
    var i = 0;
    var it = 0;
    log("Starting Simple Local Search")
    log("Starting Violation: "+objective.value)
    if(m.vars.length>0){
      while(improving > 0 && m.getWatch() < MaxTimeMilli){
        val currentVar = m.vars(i);
        if(violation(i).value > 0){
          val k = selectMin(currentVar.getDomain())(k=> objective.assignVal(currentVar,k))
          if(k!=currentVar.value){
            val obj = objective.value
            currentVar := k;
            it+=1;
            if(objective.value < obj){
              lastImproved = i;
              improving = 3
            }
          }
        }
        i+=1;
        if(i==m.vars.length){
          i=0;
          log("turned around "+objective.value)
        }
        if(i==lastImproved)improving -= 1;
      }
    }
    if(m.c.violation.value==0){
      m.handleSolution()
      log("Found Solution in "+m.getWatch());
      solutionList +:= (m.getWatch(), 0, 0, m.getSolution())
    }
    log("Done Simple Local Search")
    log("Ending Violation: "+objective.value)
    log("Nb Moves: "+it)
    
    solutionList
  }
}
class NeighbourhoodSearchOPT(val m:FZCBLSModel, objLB:Int,
    MaxTimeMilli: Int) extends SearchProcedure {
    val neighbourhoods: List[Neighbourhood] = m.neighbourhoods 
  //def apply = {
    var solutionList: List[(Long, Int, Int, String)] = List.empty[(Long, Int, Int, String)]
    val searchVariables = neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables()).toArray
    val variableMap = (0 until searchVariables.length).foldLeft(Map.empty[CBLSIntVar, Int])((acc, x) => acc + (searchVariables(x) -> x));
    val violationArray: Array[CBLSIntVar] = searchVariables.map(m.c.violation(_)).toArray;
    val tabu: Array[CBLSIntVar] = searchVariables.map(v => CBLSIntVar(m.m, 0, Int.MaxValue, 0, "Tabu_" + v.name)).toArray;
    val it = CBLSIntVar(m.m, 0, Int.MaxValue, 1, "it");
    val nonTabuVariables: CBLSSetVar = SelectLEHeapHeap(tabu, it);

    val MaxTenure = (searchVariables.length * 0.6).toInt;
    val MinTenure = 2 + 0 * (searchVariables.length * 0.1).toInt;
    val tenureIncrement = Math.max(1, (MaxTenure - MinTenure) / 10);
    var tenure = MinTenure //searchVariables.length / 8;

    val baseSearchSize = 100;
    val searchFactor = 20;
    val log = m.log
  //}
    // println(searchVariables.length);
    //m.close();
  override def run(): List[(Long, Int, Int, String)] = {
    var extendedSearch = true;

    var roundsWithoutSat = 0;
    val maxRounds = 2;

    var bestNow = Int.MaxValue;
    var best = bestNow;
    var itSinceBest = 0;
    var numOfMaxTenure = 0;
    var hasBeenSatisfied = false;
    var bestViolation = Int.MaxValue;

    var timeOfBest = m.getWatch();
    var itOfBalance = 0;
    var minViolationSinceBest = Int.MaxValue;
    var minObjectiveSinceBest = Int.MaxValue;
    var lastMinObjective = Int.MinValue;
    var hasWaited = false;
    m.objective.objectiveWeight := 0;

    var wait = 0;
    val waitDec = 1;
  

    while (!(m.c.violation.value==0 && m.objective.objectiveVar.value==objLB) && m.getWatch() < MaxTimeMilli) {
      if(it.value%10==0){
        log("it: "+it.value+" violation: "+m.c.violation.value+" objective: "+m.objective.objectiveVar.value)
       // log("viol:" + c.violatedConstraints.mkString("\n"))
      }
      //
        //showViolatedConstraints(c);
      //}
      val oldViolation = m.objective.objective.value;
      val nonTabuSet = nonTabuVariables.value.map(searchVariables(_));
      val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
        if (extendedSearch) {
          n.getExtendedMinObjective(it.value, nonTabuSet/*, Int.MinValue*/)
        } else {
          n.getMinObjective(it.value, nonTabuSet)
        }))(_.value)
        /*if(bestNeighbour.isInstanceOf[NoMove]){
          log("%%% NO MOVE "+(-bestNeighbour.value))
        }*/
     // log("Move: "+bestNeighbour.toStringMove(it.value))
        if(bestNeighbour!=null)
          bestNeighbour.commit();
        else
          log("No move exists!");
      val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
      for (v <- modifiedVars) {
        val index = variableMap(v);
        //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
        //tabu(index) := it.value + tenure;
        tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
      }
      it ++;
      //println(best + " " + bestNow + " " + bestViolation + " " + c.violation.value + " " + objective.objective.value + " - " + tenure + " " + itSinceBest)
      if (wait > 0) {
        wait -= waitDec;
        hasWaited = true;
        //  println("Waiting : " + wait)
      } else {
        itSinceBest += 1;
      }
      if (!hasBeenSatisfied) {
        //The first priority is to satisfy the problem, then minimize it.

        if (m.c.violation.value < bestViolation) {
          bestViolation = m.c.violation.value
          roundsWithoutSat = 0;
          if (m.c.violation.value == 0) {
            m.objective.objectiveWeight := 1;
            m.objective.violationWeight := 1;
            itOfBalance = it.value
            hasBeenSatisfied = true;
            bestNow = m.objective.objective.value;
            lastMinObjective = bestNow
            best = bestNow;
            timeOfBest = m.getWatch();
            val solution = m.getSolution();
            solutionList +:= (m.getWatch(), best, it.value, solution)
            m.handleSolution();
          }
          itSinceBest = 0;
          tenure = Math.max(MinTenure, tenure - 1)
          if (tenure == MinTenure) {
            extendedSearch = false;
          }
        }
        if (m.c.violation.value > bestViolation * 10) {
          extendedSearch = true;
        }
        if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
          extendedSearch = true;
          itSinceBest = 0;
          tenure = Math.min(MaxTenure, tenure + tenureIncrement);
          if (tenure == MaxTenure) {
            //Wait will be long enough to clear the tabu list.
            wait = tenure + baseSearchSize;
            bestViolation = Int.MaxValue
            tenure = MinTenure;
            roundsWithoutSat += 1;
            if (roundsWithoutSat >= maxRounds) {
              val maxViolatingNeighbourhood = selectMax(neighbourhoods, (n: Neighbourhood) => n.violation())
              maxViolatingNeighbourhood.reset();
              roundsWithoutSat = 0;
            }
          }
        }
      } else {
        // Minimize the problem
        // There are two special cases to look out for here.
        // 1) The violation is within such a small range (compared with the objective) that the violation is ignored by the search.
        //	- This shows when the violation is above 0 for a long time (possibly forever) and the objective is at a "good" low value
        // 2) The violation can grow so quickly that it overshadows the objective (ie the opposite of 1).
        //  - This shows when the violation is 0 for a long time (possibly forever) and the objective does not decrease
        //
        // There is of course also the problem of the dynamic tenure behaving badly but that is waaaaay harder to detect and do something about.
        minViolationSinceBest = Math.min(minViolationSinceBest, m.c.violation.value)
        minObjectiveSinceBest = Math.min(minObjectiveSinceBest, m.objective.getObjectiveValue())
        if (m.objective.getObjectiveValue() < bestNow || (m.c.violation.value == 0 && m.objective.getObjectiveValue() < best)) {
          bestNow = m.objective.getObjectiveValue()
          tenure = Math.max(MinTenure, tenure - 1)
          if (m.c.violation.value == 0 && bestNow < best) {
            best = bestNow;
            timeOfBest = m.getWatch();
            itOfBalance = it.value
            minViolationSinceBest = Int.MaxValue
            minObjectiveSinceBest = Int.MaxValue
            lastMinObjective = bestNow;
            tenure = Math.max(MinTenure, tenure / 2)
            val solution = m.getSolution();
            solutionList +:= (m.getWatch(), best, it.value, solution)
            m.handleSolution();
          }
          itSinceBest = 0;
        }
        //println(it.value - itOfBalance + " " + objectiveWeight.value + " " + violationWeight.value)
        if (it.value - itOfBalance > baseSearchSize * 2 && wait == 0) {
          if (minViolationSinceBest > 0) { // 1)
            m.objective.increaseViolationWeight(minViolationSinceBest)
            hasWaited = false
          } else if (bestNow <= lastMinObjective) { // 2)
            m.objective.increaseObjectiveWeight(minObjectiveSinceBest)
            hasWaited = false
          }
          lastMinObjective = bestNow;
          minViolationSinceBest = Int.MaxValue
          minObjectiveSinceBest = Int.MaxValue

          itOfBalance = it.value;
        }
        if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
          extendedSearch = true;
          itSinceBest = 0;
          tenure = Math.min(MaxTenure, tenure + tenureIncrement);
          if (tenure == MaxTenure) {
            //Wait will be long enough to clear the tabu list.
            if (m.getWatch() - timeOfBest > MaxTimeMilli / 4) {
              //println("% Reset");
              timeOfBest = m.getWatch();
              for (n <- neighbourhoods)
                n.reset();
            }
            wait = tenure + baseSearchSize;
            tenure = MinTenure;
            bestNow = m.objective.getObjectiveValue()
          }
        }
      }

    }
    solutionList +:= (m.getWatch(), -1, it.value, "Done")
    solutionList;
  }
}

class NeighbourhoodSearchSAT(val m:FZCBLSModel, MaxTimeMilli: Int) extends SearchProcedure {
  val neighbourhoods: List[Neighbourhood] = m.neighbourhoods 
  var solutionList: List[(Long, Int, Int, String)] = List.empty[(Long, Int, Int, String)]
  val searchVariables = neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[CBLSIntConstDom])).toArray
  val variableMap = (0 until searchVariables.length).foldLeft(Map.empty[CBLSIntVar, Int])((acc, x) => acc + (searchVariables(x) -> x));
  val violationArray: Array[CBLSIntVar] = searchVariables.map(m.c.violation(_)).toArray;
  val tabu: Array[CBLSIntVar] = searchVariables.map(v => CBLSIntVar(m.m, 0, Int.MaxValue, 0, "Tabu_" + v.name)).toArray;
  val it = CBLSIntVar(m.m, 0, Int.MaxValue, 1, "it");
  val nonTabuVariables: CBLSSetVar = SelectLEHeapHeap(tabu, it);

  val MaxTenure = (searchVariables.length * 0.6).toInt;
  val MinTenure = 2 + 0 * (searchVariables.length * 0.1).toInt;
  val tenureIncrement = Math.max(1, (MaxTenure - MinTenure) / 10);
  var tenure = MinTenure //searchVariables.length / 8;
  //    println(searchVariables.length);
  //m.close();
  //log("Closed Model")
    
  val log = m.log
  override def run(): List[(Long, Int, Int, String)] = {
    var extendedSearch = true;

    var roundsWithoutSat = 0;
    val maxRounds = 5;

    var bestNow = Int.MaxValue;
    var best = bestNow;
    var itSinceBest = 0;
    var itSinceMaxTenure = 0;
    var bestViolation = Int.MaxValue
    var hasWaited = false;

    val baseSearchSize = 100;
    val searchFactor = 20;
    var wait = 0;
    val waitDec = 1;
    while (m.c.violation.value != 0 && m.getWatch() < MaxTimeMilli) {
      if(it.value%10==0){
        log("it: "+it.value+" violation: "+m.c.violation.value)
      }
      val oldViolation = m.objective.objective.value;
      val nonTabuSet = nonTabuVariables.value.map(searchVariables(_));
      val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
        if (extendedSearch) {
          n.getExtendedMinObjective(it.value, nonTabuSet/*, bestNow*/)
        } else {
          n.getMinObjective(it.value, nonTabuSet)
        }))(_.value)
      if(bestNeighbour!=null)
          bestNeighbour.commit();
        else
          log("No move exists!");
      val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
      for (v <- modifiedVars) {
        val index = variableMap(v);
        //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
        //tabu(index) := it.value + tenure;
        tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
      }
      it ++;
      //println(best + " " + bestNow + " " + bestViolation + " " + c.violation.value + " " + objective.objective.value + " - " + tenure + " " + itSinceBest)

      if (wait > 0) {
        wait -= waitDec;
        hasWaited = true;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        if (m.c.violation.value == 0) {
          bestNow = m.objective.objective.value;
          best = bestNow;
          val solution = m.getSolution();
          solutionList +:= (m.getWatch(), best, it.value, solution)
          m.handleSolution();
        }
        itSinceBest = 0;
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
      }
      if (m.c.violation.value > bestViolation * 10) {
        extendedSearch = true;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
         // println("% "+roundsWithoutSat)
          if (roundsWithoutSat >= maxRounds) {
           // println("% ------------------------------------------------")
            //val maxViolating = selectMax(neighbourhoods, (n: Neighbourhood) => n.violation())
            //maxViolating.reset();
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
            //println("% "+bestViolation);
          }
        }
      }
    }
    solutionList +:= (m.getWatch(), -1, it.value, "Done")
    solutionList;
  }
}