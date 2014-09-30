package oscar.cp.constraints.sets

import oscar.cp.core.CPSetVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPIntervalVar

/** 
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */

class Excludes(val set: CPSetVar, elem: Int) extends Constraint(set.store, "Set excludes") {
  override def setup(l: CPPropagStrength): CPOutcome = set.excludes(elem)
}

class ExcludeElem(set: CPSetVar, elem: Int, b: CPBoolVar) extends Constraint(set.store, "RequiredElem") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = propagate()
    if (outcome == Failure) Failure
    else if (outcome == Success) Success
    else {
      set.callValExcludedWhenExcludedValue(this)
      set.callValRequiredWhenRequiredValue(this)
      b.callValBindWhenBind(this)
      Suspend
    }
  }

  override def propagate(): CPOutcome = {
    if (b.isBound) valBind(b)
    else if (set.isRequired(elem)) setFalse()
    else if (!set.isPossible(elem)) setTrue()
    else Suspend
  }

  @inline
  private def setTrue(): CPOutcome = {
    if (b.assign(1) == Failure) Failure
    else Success
  }

  @inline
  private def setFalse(): CPOutcome = {
    if (b.assign(0) == Failure) Failure
    else Success
  }

  @inline
  private def requires(elem: Int): CPOutcome = {
    if (set.requires(elem) == Failure) Failure
    else Success
  }
  
  @inline
  private def excludes(elem: Int): CPOutcome = {
    if (set.excludes(elem) == Failure) Failure
    else Success
  }

  override def valRequired(cpSet: CPSetVar, reqElem: Int): CPOutcome = {
    if (reqElem == elem) setFalse()
    else Suspend
  }

  override def valExcluded(cpSet: CPSetVar, exElem: Int): CPOutcome = {
    if (exElem == elem) setTrue()
    else Suspend
  }

  override def valBind(cpVar: CPIntervalVar): CPOutcome = {
    if (b.isTrue) excludes(elem)
    else requires(elem)
  }
}

object Excludes {
  def apply(set: CPSetVar, elem: Int, reifBool: CPBoolVar): Constraint = new ExcludeElem(set, elem, reifBool)
  def apply(set: CPSetVar, elem: Int): Constraint = new Excludes(set, elem)
}