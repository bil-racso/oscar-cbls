package oscar.dfo.algo

import oscar.dfo.utils.DFOPoint

/** A comparator used by DFO algorithms to compare two points and allow
  * the algorithm progress. */
trait DFOComparator {
  /** Compares two points and returns the one which is "the best" of the two.
    * 
    * @param x1 A DFOPoint     */
  def compare(x1: DFOPoint, x2: DFOPoint): DFOPoint
}