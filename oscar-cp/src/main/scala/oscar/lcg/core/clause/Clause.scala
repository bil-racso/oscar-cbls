package oscar.lcg.core.clause

import oscar.algo.ArrayStack
import oscar.lcg.core.Literal
import oscar.lcg.core.CDCLStore

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class Clause {
  
  // True if the clause is active
  private[this] var _activate: Boolean = true
  
  // Activity of the clause
  private[this] var _activity: Double = 0
  
  /** Return true if the clause is active. */
  @inline final def isActive: Boolean = _activate
  
  /** Remove the clause. */
  final def deactive(): Unit = _activate = false
  
  /** Return the activity of the clause. */
  @inline final def activity: Double = _activity
  
  /** Change the activity of the clause. */
  final def activity_=(a: Double): Unit = _activity = a

  /** Propagate the clause with no assumption. */
  def setup(): Boolean
  
  /** Propagate the clause when !literal becomes false. */
  def propagate(literal: Literal): Boolean
  
  /** Explains the last assertion. */
  def explainUnit(outReason: ArrayStack[Literal]): Unit
  
  /** Explains the last fail. */
  def explainFail(outReason: ArrayStack[Literal]): Unit
}

object Clause {
  def apply(store: CDCLStore, literals: Array[Literal], learnt: Boolean): Clause = {
    val length = literals.length
    length match { // tableswitch
      case 0 => sys.error("empty clause")
      case 1 => new UnaryClause(store, literals(0), learnt)
      //case 2 => new BinaryClause(store, literals(0), literals(1), learnt)
      case _ => new NaryClause(store, literals, learnt)
    }
  }
}