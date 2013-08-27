package oscar.cp.minizinc

class VarState (
    val name: String,
    val output: Boolean,
    val array: Boolean,
    val first: Boolean,
    val last: Boolean,
    val size: Any){

  def printArray() {
    size match {
      case x:Range => print("array1d(" + x.min + ".."+x.max)
      case x:List[Int] =>
      case x:Int => print("array1d(1.."+x)
      case _ =>
    }
  }
}