package oscar.cp.constraints.sat

import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPOutcome

abstract class Literal {
  protected val boolean: CPBoolVar
  def isTrue: Boolean = boolean.isTrue
  def isBound: Boolean = boolean.isBound
  def setTrue: CPOutcome
  def setFalse: CPOutcome
  val negation: Literal
}
