package oscar.cbls.invariants.core.algo.seq

import oscar.cbls.invariants.core.algo.fun.UpdateableFunction
import oscar.cbls.invariants.core.algo.rb.RedBlackTree

/*
abstract class UniqueIntSequenceWithPosition(maxSize:Int) extends UniqueIntSequence(maxSize){


  private val valueToUncorrectedPosition:Array[Int] = Array.fill[Int](maxSize)(-1)

  private val uncorrectedPositionToValue:Array[Int] = Array.fill[Int](maxSize)(-1)


  //internal position => external position
  private var uncorrectedToCorrectedPosition:UpdateableFunction = new UpdateableFunction
  private var correctedToUncorrectedPosition:UpdateableFunction = new UpdateableFunction



  def correctForInsert(value:Int,after:Int){

  }
  def valueAtPosition(position:Int):Int = {
    correctedToUncorrectedPosition.getBiggestLower(position) match{
      case None => uncorrectedPositionToValue(position)
      case Some(pivot) => uncorrectedPositionToValue(pivot.correction(position))
    }
  }

  def regularizePositions

  def positionOfValue(value:Int):Int = {
    val uncorrectedPoition:Int = valueToUncorrectedPosition(value)
    uncorrectedToCorrectedPosition.getBiggestLower(uncorrectedPoition) match{
      case None =>  uncorrectedPoition
      case Some(pivot) => pivot.correction(uncorrectedPoition)
    }
  }


}

*/
/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */