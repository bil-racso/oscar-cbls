package oscar.cbls.invariants.core.algo.quick


class QArrayList[@specialized T](initialLength:Int)(implicit val X:Manifest[T]) {
  var internalArray:Array[T] = new Array[T](initialLength)
  var size:Int = 0
  var maxSize = initialLength
  val nullT = null.asInstanceOf[T]

  def put(elem:T): Unit ={
    if(size == maxSize) {
      //need to double the size of the array

      maxSize *= 2
      val newArray = new Array[T](maxSize)

      var toMove = size
      while (toMove != 0) {
        toMove -= 1
        newArray(toMove) = internalArray(toMove)
      }

      internalArray = newArray
    }
    internalArray(size) = elem
    size += 1
  }

  /**returns null if was empty*/
  def pop():T = {
    if(size == 0){
      nullT
    }else{
      size -=1
      internalArray(size)
    }
  }

  def isEmpty = size == 0
}
