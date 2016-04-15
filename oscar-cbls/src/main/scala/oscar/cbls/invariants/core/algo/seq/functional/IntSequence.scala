package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.rb.RedBlackTree

/**
 * Created by rdl on 14-04-16.
 */
class IntSequence(internalPositionToValue:RedBlackTree[Int], valueToInternalPosition:RedBlackTree[Int], internalToExternalPosition:FunctionalBijection)
  extends Iterable[Int] {

  def valueAtPosition(position:Int):Int

  def positionOfValue(value:Int):Int

  def crawlerAtPosition(position:Int):IntSequenceCrawler

  def insertAfter(value:Int, pos:Int)
  def insertBefore(value:Int, pos:Int)
  def moveAfter(startPosition:Int, endPosition:Int, moveAfterPosition:Int, flip:Boolean)

}

class IntSequenceCrawler(sequence:IntSequence){
  def next:Option[IntSequenceCrawler]
  def prev:Option[IntSequenceCrawler]
  def position:Int
  def value:Int
}
