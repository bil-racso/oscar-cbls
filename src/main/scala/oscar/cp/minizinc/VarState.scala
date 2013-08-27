package oscar.cp.minizinc

import oscar.cp.minizinc.FZType._

class VarState (
    val name: String,
    val output: Boolean,
    val array: Boolean,
    val first: Boolean,
    val last: Boolean,
    val size: Any,
    val tp: FZType){

  def printArray() {
    size match {
      case x:Range => print(" = array1d(" + x.min + ".."+x.max)
      case x:List[Int] =>
      case x:Int => print(" = array1d(1.."+x)
      case _ =>
    }
  }
}