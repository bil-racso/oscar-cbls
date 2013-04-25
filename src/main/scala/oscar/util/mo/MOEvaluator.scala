package oscar.util.mo

import scala.collection.mutable.HashMap

class MOEvaluator[E <% Ordered[E]](var evalFunctions: Array[Double] => Array[E], val unfeasibleValue: Array[E]) {
  val evaluations = HashMap[Array[Double], MOOPoint[E]]()
  var nbCallToEvalFunction = 0
  
  def eval(coordinates: Array[Double], feasibleReg: FeasibleRegion): MOOPoint[E] = {
    evaluations.get(coordinates) match {
      case Some(point) => point
      case _ => {
        val newEvals = if (!feasibleReg.isFeasible(coordinates)) {
          unfeasibleValue
        }
        else {
          evalFunctions(coordinates)
        }
        val newPoint = MOOPoint(coordinates, newEvals)
        evaluations += coordinates -> newPoint
        nbCallToEvalFunction += 1
        newPoint
      }
    }
  }
}

object MOEvaluator {
  def apply[E <% Ordered[E]](evalFunctions: Array[Double] => Array[E], unfeasibleValue: Array[E]) = new MOEvaluator(evalFunctions, unfeasibleValue)
}