package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{InvariantHelper, Invariant, IntVar}

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 18/10/12
 * Time: 14:44
 * To change this template use InstanceVRP | Settings | InstanceVRP Templates.
 */

/*
  Maintains the predecessors of nodes.
  Convention:
    - 0 to N-1 for routed node.
    - N for unrouted node.
 */
case class Predecessor(next:Array[IntVar]) extends Invariant{

  val N = next.length
  registerStaticAndDynamicDependencyArrayIndex(next)
  finishInitialization();
  val preds = for(i<- 0 until N) yield new IntVar(model, 0, N, i, "preds" + i)
  for(p <- preds) p.setDefiningInvariant(this)

  def length = N
  def apply(i:Int) = preds(i)

  override def notifyIntChanged(v:IntVar,index:Int,OldVal:Int,NewVal:Int){
    assert(next(index) == v)
    // it unroutes a node
    if(NewVal == N) preds(index) := N
    else preds(NewVal) := index
  }

  override def checkInternals(){
    for(n<- 0 until N){
      //n is unrouted
      if(next(n).value==N) assert(preds(n).value==N)
      // n is routed
      else  assert(n == preds(next(n).value).value)
      }
  }

  override def toString()={
    var toReturn = ""
    toReturn +="\npreds array: ["
    for (v <- preds){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn
  }
}

