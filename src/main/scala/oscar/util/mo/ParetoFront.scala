package oscar.util.mo

abstract class ParetoFront[T, E] {
  /** Inserts a point in the Pareto Front such that the Pareto Front still contains only non-dominated points */
  def insert(candidatePoint: MOOPoint[T, E])
  /** Returns the number of points in the Pareto front dominating the candidate point */
  def nbPointsDominating(candidatePoint: MOOPoint[T, E]): Int
  /** Returns the number of points in the Pareto front dominated by the candidate point */
  def nbPointsDominated(candidatePoint: MOOPoint[T, E]): Int
  /** Returns the score of a point with regards to the current Pareto front */
  def score(point: MOOPoint[T, E]): Int
  /** A set with all non dominated points in the tree */
  def toSet: Set[MOOPoint[T, E]]
  /** Returns the number of points in the archive */
  def size: Int
  
  /** Compares two points and returns the best with regards to the current Pareto front */
  def cmpMOO(point1: MOOPoint[T, E], point2: MOOPoint[T, E]): MOOPoint[T, E] = {
    if (score(point1) >= score(point2)) point1
    else point2
  }
}