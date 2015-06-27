package oscar.cp.core.delta

import oscar.cp.core.variables.CPIntVarAdaptable
import oscar.cp.core.variables.CPIntVar

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class DeltaIntVarAdaptable(x: CPIntVar, final override val id: Int) extends DeltaIntVar {

  private[this] var _oldMin: Int = x.min
  private[this] var _oldMax: Int = x.max
  private[this] var _oldSize: Int = x.size
  
  @inline final override def oldMin: Int = _oldMin
  @inline final override def oldMax: Int = _oldMax
  @inline final override def oldSize: Int = _oldSize
  @inline final override def variable: CPIntVar = x
  @inline final override def changed: Boolean = x.size != _oldSize
  @inline final override def size: Int = _oldSize - x.size
  @inline final override def values: Iterator[Int] = x.delta(_oldMin, _oldMax, _oldSize)
  @inline final override def fillArray(values: Array[Int]): Int = x.fillDeltaArray(_oldMin, _oldMax, _oldSize, values)
  @inline final override def minChanged: Boolean = x.min != _oldMin
  @inline final override def maxChanged: Boolean = x.max != _oldMax
  
  final override def update(): Unit = {
    _oldMin = x.min
    _oldMax = x.max
    _oldSize = x.size
  }
}