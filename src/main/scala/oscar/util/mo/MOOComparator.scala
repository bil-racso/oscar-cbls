package oscar.util.mo

class MOOComparator[T, E <% Ordered[E]](
	evalCmp: MOOComparator.EvalCmp[E],
    mooPointCmp: MOOComparator.MOOPointCmp[T, E],
    pointArchiveCmp: MOOComparator.PointArchiveCmp[T, E]
    ) {
  
  def hasEvalBetterAt(point1: MOOPoint[T, E], point2: MOOPoint[T, E], functionIndex: Int): Boolean = {
    evalCmp(point1.getEvaluation(functionIndex), point2.getEvaluation(functionIndex))
  }
  
  def dominates(point1: MOOPoint[T, E], point2: MOOPoint[T, E]): Boolean = {
    mooPointCmp(point1, point2, evalCmp)
  }
  
  def dominated(point1: MOOPoint[T, E], point2: MOOPoint[T, E]): Boolean = {
    dominates(point2, point1)
  }
  
  def isEquivalent(point1: MOOPoint[T, E], point2: MOOPoint[T, E]): Boolean = {
    !dominates(point1, point2) && ! dominates(point2, point1)
  }
  
  def cmpWithArchive(point1: MOOPoint[T, E], point2: MOOPoint[T, E], archive: ParetoFront[T, E]): Boolean = {
    pointArchiveCmp(point1, point2, archive)
  }
}

object MOOComparator {
  type EvalCmp[E] = (E, E) => Boolean
  type MOOPointCmp[T, E] = (MOOPoint[T, E], MOOPoint[T, E], EvalCmp[E]) => Boolean
  type PointArchiveCmp[T, E] = (MOOPoint[T, E], MOOPoint[T, E], ParetoFront[T, E]) => Boolean
  
  def apply[T, E <% Ordered[E]](evalCmp: EvalCmp[E], mooPointCmp: MOOPointCmp[T, E], pointArchiveCmp: PointArchiveCmp[T, E]) = new MOOComparator(evalCmp, mooPointCmp, pointArchiveCmp)
  
  def smaller[E <% Ordered[E]](functionEval1: E, functionEval2: E) {
    functionEval1 < functionEval2
  }
  
  def bigger[E <% Ordered[E]](functionEval1: E, functionEval2: E): Boolean = {
    functionEval1 > functionEval2
  }
  
  def strongDominance[T, E <% Ordered[E]](point1: MOOPoint[T, E], point2: MOOPoint[T, E], evalCmp: EvalCmp[E]): Boolean = {
    var nbBetterEvals = 0
    var nbEquivEvals = 0
    for (functionIndex <- 0 until point1.nbEvaluations) {
      if (evalCmp(point1.getEvaluation(functionIndex), point2.getEvaluation(functionIndex))) nbBetterEvals += 1
      else if(!evalCmp(point2.getEvaluation(functionIndex), point1.getEvaluation(functionIndex))) nbEquivEvals +=1
      else return false
    }
    (nbBetterEvals + nbEquivEvals == point1.nbEvaluations) && nbBetterEvals > 0
  }
  
  def cmpScore[T, E <% Ordered[E]](point1: MOOPoint[T, E], point2: MOOPoint[T, E], archive: ParetoFront[T, E], mooPointCmp: MOOPointCmp[T, E], evalCmp: EvalCmp[E]): Boolean = {
    if (mooPointCmp(point1, point2, evalCmp)) true
    else if (mooPointCmp(point1, point2, evalCmp)) false
    else {
      archive.score(point1, mooPointCmp) >= archive.score(point2, mooPointCmp)
    }
  }
}

