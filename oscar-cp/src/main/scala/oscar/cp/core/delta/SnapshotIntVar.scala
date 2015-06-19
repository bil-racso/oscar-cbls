package oscar.cp.core.delta

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class SnapshotIntVar extends Snapshot {
  def oldMin: Int
  def oldMax: Int
  def oldSize: Int
  def changed: Boolean
  def size: Int
  def values: Iterator[Int]
  def fillArray(values: Array[Int]): Int
  def minChanged: Boolean
  def maxChanged: Boolean
}