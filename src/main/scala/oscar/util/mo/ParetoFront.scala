package oscar.util.mo

abstract class ParetoFront[E <% Ordered[E]] {
  /** Inserts a point in the Pareto Front such that the Pareto Front still contains only non-dominated points */
  def insert(candidate: ArchiveElement[E], comparator: MOOComparator[E]): Boolean
  /** Returns the number of points in the Pareto front dominating the candidate point */
  def nbPointsDominating(candidatePoint: MOOPoint[E], comparator: MOOComparator[E]): Int
  /** Returns the number of points in the Pareto front dominated by the candidate point */
  def nbPointsDominated(candidatePoint: MOOPoint[E], comparator: MOOComparator[E]): Int
  /** Returns the score of a point with regards to the current Pareto front */
  def score(point: MOOPoint[E], mooPointCmp: MOOComparator.MOOPointCmpRaw[E]): Int
  /** A set with all non dominated points in the tree */
  def toSet: Set[MOOPoint[E]]
  /** A list with all non dominated points in the tree */
  def toList: List[MOOPoint[E]]
  /** Returns the number of points in the archive */
  def size: Int
  /** Returns true if the point is contained in the Pareto front */
  def contains(mooPoint: MOOPoint[E]): Boolean
  /** Returns a random element from the Pareto front */
  def randomElement: ArchiveElement[E]
  /** Removes the specified element from the Pareto front */
  def removeElement(element: ArchiveElement[E]): Boolean
  /** Returns the first element of the archive */
  def head: ArchiveElement[E]
  /** Returns true if the Pareto front contains no point, false otherwise */
  def isEmpty: Boolean
  /** Returns an array containing the points which are the biggest in a dimension */
  def getExtremePoints: Array[(ArchiveElement[E], ArchiveElement[E])]
  /** Returns the point that is the closest to the specified coordinates */
  def getClosest(coordinates: Array[Double]): ArchiveElement[E]
}