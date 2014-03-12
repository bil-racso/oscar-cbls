package oscar.cp.core.domains

import oscar.cp.core.CPOutcome

/** @author Renaud Hartert */

abstract class CPIntDomain extends Iterable[Int] {
  
  def size: Int

  def isEmpty: Boolean

  def max: Int

  def min: Int
  
  def hasValue(value: Int): Boolean
  
  def removeValue(value: Int): CPOutcome 
  
  def assign(value: Int): CPOutcome

  def updateMin(value: Int): CPOutcome

  def updateMax(value: Int): CPOutcome

  def nextValue(value: Int): Int

  def prevValue(value: Int): Int

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] 
}