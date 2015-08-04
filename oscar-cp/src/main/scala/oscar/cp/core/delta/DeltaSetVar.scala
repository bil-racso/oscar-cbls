package oscar.cp.core.delta

import oscar.cp.core.variables.CPSetVar

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class DeltaSetVar(x: CPSetVar) extends Delta {
  
  private[this] var _oldSizePossible: Int = x.possibleSize
  private[this] var _oldSizeRequired: Int = x.requiredSize
  
  @inline final def oldSizePossible: Int = _oldSizePossible
  @inline final def oldSizeRequired: Int = _oldSizeRequired
  
  final override def update() {
    _oldSizePossible = x.possibleSize
    _oldSizeRequired = x.requiredSize
  }
}