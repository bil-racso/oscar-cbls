package oscar.cp.linearizedDFS.branching

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.linearizedDFS.{Assign, Remove}
import oscar.cp._
import oscar.algo.search.Alternative

/**
 * Created by saschavancauwelaert on 09/12/14.
 */
class BinaryBranching(vars: Array[CPIntVar], varHeuris: Int => Int, valHeuris: Int => Int) extends oscar.cp.searches.BinaryBranching(vars,varHeuris,valHeuris) {
  
  def this(vars: Array[CPIntVar], varHeuris: (Int => Int)) = this(vars,varHeuris,minVal(vars))

  override def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => {
        val i = nextVar()
        val v = valHeuris(i)
          Seq(Assign(vars(i), v), Remove(vars(i), v))
      }
    }
  }

}

class BinaryFirstFailBranching(x: Array[CPIntVar], valHeuris: Int => Int) extends BinaryBranching(x, minDom(x), valHeuris) {
  
  def this(x: Array[CPIntVar]) = this(x,minVal(x))
  
  def this(x: CPIntVar*) = this(x.toArray)
}

class BinaryStaticOrderBranching(vars: Array[_ <: CPIntVar], valHeuris: Int=> Int) extends Branching {
  
  def this(vars: Array[_ <: CPIntVar]) = this(vars,minVal(vars.toArray))

  val cp = vars(0).store
  var y = vars.asInstanceOf[Array[CPIntVar]]
  var i = new ReversibleInt(cp, 0)

  override def alternatives(): Seq[Alternative] = {

    while (i.value < y.size && y(i.value).isBound) { i.incr() }

    if (i.value < y.size) {

      val x: CPIntVar = y(i.value)
      val v = valHeuris(i.value)
      Seq(Assign(x, v), Remove(x, v))

    } else {
      noAlternative
    }
  }
}
