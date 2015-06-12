package oscar.cp.core

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPSetVar

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Snapshot {
  def update(): Unit
}

final class SnapshotVarInt(x: CPIntVar) extends Snapshot {
  
  private[this] var _oldMin: Int = x.min
  private[this] var _oldMax: Int = x.max
  private[this] var _oldSize: Int = x.size
  
  @inline final def oldMin: Int = _oldMin
  @inline final def oldMax: Int = _oldMax
  @inline final def oldSize: Int = _oldSize
  
  final override def update(): Unit = {
    _oldMin = x.min
    _oldMax = x.max
    _oldSize = x.size
  }
}

final class SnapshotVarSet(x: CPSetVar) extends Snapshot {
  
  private[this] var _oldSizePossible: Int = x.possibleSize
  private[this] var _oldSizeRequired: Int = x.requiredSize
  
  @inline final def oldSizePossible: Int = _oldSizePossible
  @inline final def oldSizeRequired: Int = _oldSizeRequired
  
  final override def update() {
    _oldSizePossible = x.possibleSize
    _oldSizeRequired = x.requiredSize
  }
}