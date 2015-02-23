package oscar.cbls.invariants.core.algo.QuickList

class QList[T](val head:T, val tail:QList[T] = null){
  def size:Int = {
    var curr = this.tail
    var toReturn = 1
    while(curr != null){
      curr = curr.tail
      toReturn += 1
    }
    toReturn
  }

  def reverse:QList[T] = {
    var toReturn:QList[T] = null
    var current = this
    while(current != null){
      toReturn = new QList(current.head,toReturn)
      current = current.tail
    }
    toReturn
  }
}

