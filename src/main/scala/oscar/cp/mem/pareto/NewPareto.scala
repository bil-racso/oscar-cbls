package oscar.cp.mem.pareto

class NewPareto[Sol](val nObjs: Int) {

  val Objs = 0 until nObjs

  private var X: List[MOSol[Sol]] = List()

  def insert(xNew: MOSol[Sol]): Int = {
    var removed = 0
    def insert0(xNew: MOSol[Sol], l: List[MOSol[Sol]]): List[MOSol[Sol]] = l match {
      case Nil => xNew :: Nil
      case x :: t => {
        if (xNew dominates x) {
          removed += 1
          insert0(xNew, t)
        }
        else if (x dominates xNew) {
          removed = -1
          x :: t
        }
        else x :: insert0(xNew, t)
      }
    }    
    X = insert0(xNew, X)
    removed
  }

  def upper(i: Int, value: Int): Int = upper0(i, value, Int.MaxValue, X)

  private def upper0(i: Int, value: Int, ub: Int, l: List[MOSol[Sol]]): Int = l match {
    case Nil => ub
    case x :: t => {
      if (x.objs(i) < ub && x.objs(i) > value) upper0(i, value, x.objs(i), t)
      else upper0(i, value, ub, t)
    }
  }

  def join(set: NewPareto[Sol]) = {
    set.foreach(x => this.insert(x))
  }

  def size = X.size

  def isEmpty = X.isEmpty

  def apply(i: Int) = X(i)

  def map[B](f: (MOSol[Sol]) => B): IndexedSeq[B] = X.map(f).toIndexedSeq

  def mkString(s: String) = X.mkString(s)

  def foreach[B](f: (MOSol[Sol]) => B) = X.foreach(f)

  def clear() { X = List() }

  def filter(f: (MOSol[Sol]) => Boolean) = X.filter(f)
  
  def toArray: Array[MOSol[Sol]] = X.toArray

  def max(f: (MOSol[Sol]) => Int): MOSol[Sol] = min(-f(_))

  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    var minValue = Int.MaxValue
    var min: MOSol[Sol] = null
    for (x <- X) {
      val value = f(x)
      if (value < minValue) {
        minValue = value
        min = x
      }
    }
    min
  }
  
  override def toString: String = X.toString
}

object NewPareto {
  def apply[Sol](nObjs: Int) = new NewPareto[Sol](nObjs)
}