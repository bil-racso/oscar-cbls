package oscar.algo.search

import scala.collection.immutable.LinearSeq

/**
 *  A class to represent an iterator of alternative.
 *  Classical Iterator[Alternative] could be used but 
 *  does not convey the discrepancy information.
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Alternatives {
  
  def next(): Alternative
  
  def hasNext(): Boolean
  
  def discrepancy: Int
}

final class AlternativeIndexedSeq(alternatives: Seq[Alternative]) extends Alternatives {  
  private var i = 0 
  override final def next(): Alternative = {
    val al = alternatives(i)
    i += 1
    al
  }  
  override final def hasNext: Boolean = i < alternatives.length
  override final def discrepancy: Int = i
}

final class AlternativeLinearSeq(alternatives: Seq[Alternative]) extends Alternatives {
  private var list = alternatives
  private var d = 0
  override final def next(): Alternative = {
    val al = list.head
    list = list.tail
    d += 1
    al
  }
  override final def hasNext: Boolean = !list.isEmpty
  override final def discrepancy: Int = d
}

final class AlternativeArray(alternatives: Array[Alternative]) extends Alternatives {  
  private var i = 0 
  override final def next(): Alternative = {
    val al = alternatives(i)
    i += 1
    al
  }  
  override final def hasNext: Boolean = i < alternatives.length
  override final def discrepancy: Int = i
}

object Alternatives {
  def apply(alternatives: Seq[Alternative]): Alternatives = {
    if (alternatives.isInstanceOf[IndexedSeq[Alternative]]) new AlternativeIndexedSeq(alternatives)
    else new AlternativeLinearSeq(alternatives)
  }
  def apply(alternatives: Array[Alternative]): Alternatives = new AlternativeArray(alternatives)
}
