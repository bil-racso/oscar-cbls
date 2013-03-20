package oscar.util.tree

class PositionedNode[T](val label: T, var pos: Double, val sons: List[PositionedNode[T]], val edgeLabels: List[T]) {
  def moveTree(x: Double) = new PositionedNode(label, this.pos + x, sons, edgeLabels)
  
  def minOffset: Double = {
    def minOffsetAux(curNode: PositionedNode[T], acc: Double): List[Double] = {
      var myIntList = List(curNode.pos + acc)
      for (son <- curNode.sons) {
        myIntList :::= minOffsetAux(son, acc + curNode.pos)
      }
      myIntList
    }
    Math.abs(minOffsetAux(this, 0).min)
  }
}

object PositionedNode {
  def apply[T](label: T, pos: Double, sons: List[PositionedNode[T]], edgeLabels: List[T]) = new PositionedNode(label, pos, sons, edgeLabels)
}