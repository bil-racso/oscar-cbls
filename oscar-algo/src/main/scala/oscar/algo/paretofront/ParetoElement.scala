package oscar.algo.paretofront

abstract class ParetoElement[U](minimization: Boolean = true) {
  def coordinates: Array[U]
  
  def objectives: Array[Double]
  
  val nObjectives = objectives.length
  
  val nCoordinates = coordinates.length
  
  /** The comparison function used on each dimension to determine if a point is dominated or not. */
  def comp(a: Double, b: Double): Boolean = {
    if (minimization) a < b
    else a > b
  }
  
  /** Returns an integer +1 if this dominates other, -1 if other dominates this or
   	*  0 if other and this don't dominate each other */
  def dominance(other: ParetoElement[U]): Int = {
    def areEquals(index: Int): Boolean = {
      if (index >= this.objectives.length) true
      else if (this.objectives(index) != other.objectives(index)) false
      else areEquals(index + 1)
    }
    def dominanceAux(index: Int, dom: Boolean, nDom: Boolean): Int = {
      if (index >= this.objectives.length) {
        if (dom && nDom) return 0
        else if (dom) return 1
        else return -1
      }
      if (this.comp(this.objectives(index), other.objectives(index))) {
        if (nDom) return 0
        else dominanceAux(index + 1, true, nDom)
      }
      else if (this.objectives(index) == other.objectives(index)) {
        dominanceAux(index + 1, dom, nDom)
      }
      else {
        if (dom) return 0
        else dominanceAux(index + 1, dom, true)
      }
    }
    if (areEquals(0)) 0
    else dominanceAux(0, false, false)
  }
  
  override def toString: String = objectives.mkString("Element(", ", ", ")")
}