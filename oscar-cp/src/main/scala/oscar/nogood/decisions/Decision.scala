package oscar.nogood.decisions

import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar

abstract class Decision {
  def apply(): Unit
  def opposite: Decision
  def isTrue: Boolean
  def toLiteral: CPBoolVar
  def unary_!(): Decision = opposite
}

class Assign(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.assign(variable, value)
  override def opposite: Decision = new Remove(variable, value)
  override def isTrue: Boolean = variable.isBoundTo(value)
  override def toLiteral: CPBoolVar = variable ?== value
  override def toString: String = s"[${variable.name} == $value]"
}

class Remove(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.remove(variable, value)
  override def opposite: Decision = new Assign(variable, value)
  override def isTrue: Boolean = !variable.hasValue(value)
  override def toLiteral: CPBoolVar = variable ?!= value
  override def toString: String = s"[${variable.name} != $value]"
}

class LowerEq(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.post(variable <= value)
  override def opposite: Decision = new Greater(variable, value)
  override def isTrue: Boolean = variable.max <= value
  override def toLiteral: CPBoolVar = variable ?<= value
  override def toString: String = s"[${variable.name} <= $value]"
}

class Greater(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.post(variable > value)
  override def opposite: Decision = new LowerEq(variable, value)
  override def isTrue: Boolean = variable.min > value
  override def toLiteral: CPBoolVar = variable ?> value
  override def toString: String = s"[${variable.name} > $value]"
}