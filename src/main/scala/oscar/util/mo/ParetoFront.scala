package oscar.util.mo

abstract class ParetoFront[T, E <% Ordered[E]] {
  /** Inserts a point in the Pareto Front such that the Pareto Front still contains only non-dominated points */
  def insert(candidatePoint: MOOPoint[T, E], comparator: MOOComparator[T, E])
  /** Returns the number of points in the Pareto front dominating the candidate point */
  def nbPointsDominating(candidatePoint: MOOPoint[T, E], comparator: MOOComparator[T, E]): Int
  /** Returns the number of points in the Pareto front dominated by the candidate point */
  def nbPointsDominated(candidatePoint: MOOPoint[T, E], comparator: MOOComparator[T, E]): Int
  /** Returns the score of a point with regards to the current Pareto front */
  def score(point: MOOPoint[T, E], mooPointCmp: MOOComparator.MOOPointCmp[T, E]): Int
  /** A set with all non dominated points in the tree */
  def toSet: Set[MOOPoint[T, E]]
  /** Returns the number of points in the archive */
  def size: Int
}