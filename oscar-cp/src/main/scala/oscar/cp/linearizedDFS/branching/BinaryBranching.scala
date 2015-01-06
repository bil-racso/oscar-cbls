package oscar.cp.linearizedDFS.branching

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.core.CPIntVar
import oscar.cp.linearizedDFS.{Assign, Remove}
import oscar.cp._
import oscar.cp.searches.AbstractBinaryBranching

/**
 * Created by saschavancauwelaert on 09/12/14.
 */
class BinaryBranching[X <: CPIntVar](vars: Array[X], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int) = minVal) extends AbstractBinaryBranching(vars,varHeuris) {

  def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => {
        val y = nextVar()
        val v = valHeuris(y)
          Seq(Assign(y, v), Remove(y, v))
      }
    }
  }

}

class BinaryFirstFailBranching(x: Array[CPIntVar], valHeuris: (CPIntVar => Int) = minVal) extends BinaryBranching(x, _.size, valHeuris) {
  def this(x: CPIntVar*) = this(x.toArray)
}

class BinaryStaticOrderBranching(vars: Array[_ <: CPIntVar], valHeuris: (CPIntVar => Int) = minVal) extends Branching {

  val cp = vars(0).store
  var y = vars.asInstanceOf[Array[CPIntVar]]
  var i = new ReversibleInt(cp, 0)

  override def alternatives(): Seq[Alternative] = {

    while (i.value < y.size && y(i.value).isBound) { i.incr() }

    if (i.value < y.size) {

      val x: CPIntVar = y(i.value)
      val v = valHeuris(x)
      Seq(Assign(x, v), Remove(x, v))

    } else {
      noAlternative
    }
  }
}
