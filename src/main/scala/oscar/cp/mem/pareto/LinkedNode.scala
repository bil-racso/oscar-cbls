package oscar.cp.mem.pareto

class LinkedNode[S](v: Int, pp: ParetoPoint[S], l: OrderedLinkedList[S]) {

  private var p: LinkedNode[S] = null
  private var n: LinkedNode[S] = null

  def list = l
  def value = v

  def prev = p
  def next = n

  def point = pp

  def prev_=(x: LinkedNode[S]) { p = x }
  def next_=(x: LinkedNode[S]) { n = x }

  def isFirst = p == null
  def isLast = n == null
}