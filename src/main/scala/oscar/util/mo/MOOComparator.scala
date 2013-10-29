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
package oscar.util.mo

class MOOComparator[E <% Ordered[E]](
	evalCmp: MOOComparator.EvalCmp[E],
    mooPointCmp: MOOComparator.MOOPointCmp[E],
    pointArchiveCmp: MOOComparator.PointArchiveCmp[E]
    ) {
  
  def hasEvalBetterAt(point1: MOOPoint[E], point2: MOOPoint[E], functionIndex: Int): Boolean = {
    evalCmp(point1.getEvaluation(functionIndex), point2.getEvaluation(functionIndex))
  }
  
  def hasEvalEquivAt(point1: MOOPoint[E], point2: MOOPoint[E], functionIndex: Int): Boolean = {
    !(evalCmp(point1.getEvaluation(functionIndex), point2.getEvaluation(functionIndex)) || evalCmp(point2.getEvaluation(functionIndex), point1.getEvaluation(functionIndex)))
  }
  
  def dominates(point1: MOOPoint[E], point2: MOOPoint[E]): Boolean = {
    mooPointCmp(point1, point2, evalCmp)
  }
  
  def dominated(point1: MOOPoint[E], point2: MOOPoint[E]): Boolean = {
    dominates(point2, point1)
  }
  
  def isEquivalent(point1: MOOPoint[E], point2: MOOPoint[E]): Boolean = {
    !(dominates(point1, point2) || dominates(point2, point1))
  }
  
  def hasEqualEvals(point1: MOOPoint[E], point2: MOOPoint[E]): Boolean = {
    for(functionIndex <- 0 until point1.nbEvaluations) {
      if(!hasEvalEquivAt(point1, point2, functionIndex)) {
        return false
      }
    }
    true
  }
  
  def cmpWithArchive(point1: MOOPoint[E], point2: MOOPoint[E], archive: ParetoFront[E]): Boolean = {
    pointArchiveCmp(point1, point2, archive, mooPointCmp, evalCmp)
  }
}

object MOOComparator {
  type EvalCmp[E] = (E, E) => Boolean
  type MOOPointCmp[E] = (MOOPoint[E], MOOPoint[E], EvalCmp[E]) => Boolean
  type MOOPointCmpRaw[E] = (MOOPoint[E], MOOPoint[E]) => Boolean
  type PointArchiveCmp[E] = (MOOPoint[E], MOOPoint[E], ParetoFront[E], MOOPointCmp[E], EvalCmp[E]) => Boolean
  
  def apply[E <% Ordered[E]](evalCmp: EvalCmp[E], mooPointCmp: MOOPointCmp[E], pointArchiveCmp: PointArchiveCmp[E]) = new MOOComparator(evalCmp, mooPointCmp, pointArchiveCmp)
  
  def smaller[E <% Ordered[E]](functionEval1: E, functionEval2: E): Boolean = {
    functionEval1 < functionEval2
  }
  
  def bigger[E <% Ordered[E]](functionEval1: E, functionEval2: E): Boolean = {
    functionEval1 > functionEval2
  }
  
  def strongDominance[E <% Ordered[E]](point1: MOOPoint[E], point2: MOOPoint[E], evalCmp: EvalCmp[E]): Boolean = {
    var nbBetterEvals = 0
    var nbEquivEvals = 0
    //println(point1)
    //println(point2)
    for (functionIndex <- 0 until point1.nbEvaluations) {
      if (evalCmp(point2.getEvaluation(functionIndex), point1.getEvaluation(functionIndex))) return false
      else{
        if (evalCmp(point1.getEvaluation(functionIndex), point2.getEvaluation(functionIndex))) nbBetterEvals += 1
        else nbEquivEvals +=1
      }
    }
    //println("nbBetterEvals " + nbBetterEvals + " nbEquivEvals " + nbEquivEvals)
    (nbBetterEvals + nbEquivEvals == point1.nbEvaluations) && (nbBetterEvals > 0)
  }
  
  def cmpScore[E <% Ordered[E]](point1: MOOPoint[E], point2: MOOPoint[E], archive: ParetoFront[E], mooPointCmp: MOOPointCmp[E], evalCmp: EvalCmp[E]): Boolean = {
    if (mooPointCmp(point1, point2, evalCmp)) true
    else if (mooPointCmp(point1, point2, evalCmp)) false
    else {
      //println(point1 + "  =>  Score: " + archive.score(point1, mooPointCmp(_, _, evalCmp)))
      //println(point2 + "  =>  Score: " + archive.score(point2, mooPointCmp(_, _, evalCmp)))
      archive.score(point1, mooPointCmp(_, _, evalCmp)) >= archive.score(point2, mooPointCmp(_, _, evalCmp))
    }
  }
}

object MinMOOComparator {
  def apply[E <% Ordered[E]]() = new MOOComparator(MOOComparator.smaller[E], MOOComparator.strongDominance[E], MOOComparator.cmpScore[E])
}

object MaxMOOComparator {
  def apply[E <% Ordered[E]]() = new MOOComparator(MOOComparator.bigger[E], MOOComparator.strongDominance[E], MOOComparator.cmpScore[E])
}

