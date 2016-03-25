package oscar.cbls.invariants.core.algo.set

import oscar.cbls.invariants.core.algo.quick.QList

/*
class QSet(maxVal:Int) extends Set[Int]{
  //this is an over-approximate of elements in the set, no duplicates
  private[this] var valuesIn:QList[Int] = null
  //this is the definite thing for elements in the set
  private[this] val isValueIn:Array[Boolean] = Array.fill[Boolean](maxVal+1)(false)

  //tells is a value is already in the list or not
  private[this] val isValueInList:Array[Boolean] = Array.fill[Boolean](maxVal+1)(false)

  override def contains(elem: Int): Boolean = isValueIn(elem)

  override def +(elem: Int): Set[Int] = {
    if(!isValueIn(elem)){
      isValueIn(elem) = true
      if(!isValueInList(elem)){
        valuesIn = QList(elem,valuesIn)
        isValueInList(elem) = true
      }
    }
    this
  }

  override def -(elem: Int): Set[Int] = {
    if(isValueIn(elem)){
      isValueIn(elem) = false
    }
    this
  }

  override def iterator: Iterator[Int] = {

  }
}
*/