package oscar.cbls.invariants.core.algo.QuickList

class QList[T](val head:T, val tail:QList[T] = null){
  def length:Int = {
    var curr = this.tail
    var toReturn = 1
    while(curr != null){
      curr = curr.tail
      toReturn += 1
    }
    toReturn
  }
}

