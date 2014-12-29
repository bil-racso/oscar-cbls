package oscar.cp.core

import scala.util.Random
import oscar.cp._

/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class CPBoolVar extends CPIntVar {

  /** @return a constraint setting the boolean variable to true (1) */
  def constraintTrue(): Constraint

  /** @return a constraint setting the boolean variable to false (0) */
  def constraintFalse(): Constraint

  /** @return true if the variable is bound and bound to value 1 */
  def isTrue: Boolean

  /** @return true if the variable is bound and bound to value 0 */
  def isFalse: Boolean

  /** Logical or */
  def or(y: CPBoolVar): CPBoolVar

  /** Logical and */
  def and(y: CPBoolVar): CPBoolVar

  def not(): CPBoolVar

  def implies(y: CPBoolVar): CPBoolVar

  def unary_!(): CPBoolVar 
  
  def |(y: CPBoolVar): CPBoolVar

  def ||(y: CPBoolVar): CPBoolVar

  def &(y: CPBoolVar): CPBoolVar

  def &&(y: CPBoolVar): CPBoolVar

  def ==>(y: CPBoolVar): CPBoolVar
}

object CPBoolVar {

  /** Creates a new CP Boolean Variable */
  def apply(name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVarWrapper(CPIntVar(0 to 1)(store), name)

  /** Creates a new CP Boolean Variable */
  def apply()(implicit store: CPStore): CPBoolVar = apply("")(store)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean, name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVarWrapper(CPIntVar(if (b) 1 else 0)(store), name)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean)(implicit store: CPStore): CPBoolVar = apply(b, "")(store)

  @deprecated("use apply(b: Boolean)(implicit store: CPStore) instead", "1.0")
  def apply(cp: CPStore, b: Boolean): CPBoolVar = new CPBoolVarWrapper(CPIntVar(if (b) 1 else 0)(cp))
}  
  
