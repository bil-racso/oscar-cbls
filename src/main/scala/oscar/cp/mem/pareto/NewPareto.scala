package oscar.cp.mem.pareto

import scala.collection.mutable.LinkedList

class NewPareto[Sol](val nObjs: Int) {
  
  val Objs = 0 until nObjs
  
  val X: LinkedList[MOSol[Sol]] = LinkedList()
  
  def insert(sol: Sol, obj1: Int, obj2: Int) { 
    insert(MOSol(sol, obj1, obj2)) 
  }
  
  def insert(xNew: MOSol[Sol]) {
    X.filter(x => xNew dominates x)
  }
  
  def upper(i: Int, value: Int): Int = {
    var ub = Int.MaxValue
    for (x <- X; if (x.objs(i) < ub && x.objs(i) > value)) 
      ub = x.objs(i)
    return ub
  }
  
  def size = X.size  
  def apply(i: Int) = X(i) 
  def map[B](f: (MOSol[Sol]) => B): IndexedSeq[B] = X.map(f).toIndexedSeq
  def mkString(s: String) = X.mkString(s)
}

object NewPareto {
  def apply[Sol](nObjs: Int) = new NewPareto[Sol](nObjs)
}