package scampi.cp.constraints
import scampi.cp.core.Constraint
import scampi.cp.core._
import scampi.reversible._
import scampi.cp.core.CPOutcome
import scampi.cp.modeling._


class GCCFWC(val X: Array[CPVarInt],minval: Int, low: Array[Int], up: Array[Int]) extends Constraint(X(0).getStore(), "GCCFWC") {
  
  val nbBound = Array.tabulate(low.size)(i => new ReversibleInt(X(0).getStore,0))
  
  
  override def setup(l: CPPropagStrength): CPOutcome =  {   
    for (i <- 0 until X.length) {
      X(i).callPropagateWhenDomainChanges(this);
      X(i).callValRemoveIdxWhenValueIsRemoved(this,i)
    }
    
    CPOutcome.Suspend
  }
  
  override def propagate(): CPOutcome = {
    println("something changed:"+X.mkString(","));
    //if (X(0).removeValue(0) == CPOutcome.Failure) return CPOutcome.Failure
    
    CPOutcome.Suspend
  }
  
  
  override def valRemoveIdx(x: CPVarInt ,i: Int, v: Int): CPOutcome = {
    println("var at index"+i+"lost value"+v)
	CPOutcome.Suspend
  }
}


object GCCFWC {
  
  def main(args: Array[String]) {
	  val cp = new CPSolver()
	  var x1 = new CPVarInt(cp,0, 2)
	  var x2 = new CPVarInt(cp,0, 2)
	  val x = Array(x1,x2)
	  
	  cp.add(new GCCFWC(x,0,Array(0,1,2),Array(0,1,2)))
	  
	  cp.add(x1 != 0)
    
  }
}
