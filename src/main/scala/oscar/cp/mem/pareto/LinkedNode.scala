package oscar.cp.mem.pareto

class LinkedNode[S](v: Int, l: OrderedLinkedList[S]) {

  private var p: LinkedNode[S] = null
  private var n: LinkedNode[S] = null
  
  var pp: ParetoPoint[S] = null

  def list = l
  def value = v

  def prev = p
  def next = n

  def point = pp
  def point_= (p: ParetoPoint[S]) { pp = p}

  def prev_=(x: LinkedNode[S]) { p = x }
  def next_=(x: LinkedNode[S]) { n = x }

  def isFirst = p == null
  def isLast = n == null
}