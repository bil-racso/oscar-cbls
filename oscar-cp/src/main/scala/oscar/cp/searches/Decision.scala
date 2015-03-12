package oscar.cp.searches

import oscar.algo.search.Alternative
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPOutcome.Failure
import oscar.algo.reversible.ReversibleContext

abstract class Decision extends Alternative
abstract class DomainDecision extends Decision
abstract class ControlDecision extends Decision

final class Push(val context: ReversibleContext) extends ControlDecision {
  override def apply(): Unit = context.pushState()
  override def toString: String = s"Push"
}

final class Pop(val context: ReversibleContext) extends ControlDecision {
  override def apply(): Unit = context.pop()
  override def toString: String = s"Pop"
}

final class Remove(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = variable.store.remove(variable, value)
  override def toString: String = s"Remove(${variable.name}, $value)"
}

final class Assign(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = variable.store.assign(variable, value)
  override def toString: String = s"Assign(${variable.name}, $value)"
}

final class LowerEq(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = {
    val out = variable.updateMax(value)
    if (out == Failure) variable.store.fail()
    else variable.store.propagate()
  }
  override def toString: String = s"LowerEq(${variable.name}, $value)"
}

final class GreaterEq(val variable: CPIntVar, val value: Int) extends DomainDecision {
  override def apply(): Unit = {
    val out = variable.updateMin(value)
    if (out == Failure) variable.store.fail()
    else variable.store.propagate()
  }
  override def toString: String = s"GreaterEq(${variable.name}, $value)"
}

object Decision {
  @inline final def remove(variable: CPIntVar, value: Int): Decision = new Remove(variable, value)
  @inline final def assign(variable: CPIntVar, value: Int): Decision = new Assign(variable, value)
  @inline final def lowerEq(variable: CPIntVar, value: Int): Decision = new LowerEq(variable, value)
  @inline final def greaterEq(variable: CPIntVar, value: Int): Decision = new GreaterEq(variable, value)
  @inline final def push(context: ReversibleContext): Decision = new Push(context)
  @inline final def pop(context: ReversibleContext): Decision = new Pop(context) 
  def apply(decision: => Unit): Alternative = () => decision
}
