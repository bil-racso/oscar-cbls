package oscar.examples.cp

import oscar.cp.modeling.CPSolver
import oscar.cp.modeling._
import oscar.cp.core.CPVarInt
import oscar.cp.constraints.BinPacking
import oscar.cp.constraints.Sum
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPackingFlowExtended



object BinPackingBasic {
  def main(args: Array[String]) {
    
    val rand = new scala.util.Random(2)
    
    val wmin = 20
    val wmax = 5
    val n = 30
    val m = 10
    
    val sol = Array.tabulate(n)(i => (rand.nextInt(m),wmin+rand.nextInt(wmax)))


    val w = sol.map(_._2)
    val lsol = Array.tabulate(m)(i => sol.filter(_._1 == i).map(_._2).sum)
    val csol = Array.tabulate(m)(i => sol.filter(_._1 == i).size)
    
    def getDom(i: Int) = {
      val bin = sol(i)._1
      (Array.fill(3)(rand.nextInt(m)) :+ sol(i)._1 :+ m).toSet
    }
    
    val cp = new CPSolver()
    val x = Array.tabulate(n)(i => CPVarInt(cp,getDom(i)))
    val l = Array.tabulate(m)(j => CPVarInt(cp,(lsol(j)/2) to (lsol(j)))) :+  CPVarInt(cp,0 to w.sum)
    val c = Array.tabulate(m+1)(j => CPVarInt(cp,0 to n))
    var nbsol = 0
    var best = 0
    //cp.tim
    cp.silent = true
    cp.maximize(l(m)) subjectTo {
      
    } exploration {
      cp.deterministicBinaryFirstFail(x)
      nbsol += 1
      best = l(m).value
    } 
    
    best = 0
    nbsol = 0
   // cp.objective(l(m)).relax()
    cp.objective.objs(0).best = Int.MinValue 
    cp.runSubjectTo() {
      cp.add(new BinPacking(x, w, l))
      cp.add(new BinPackingFlowExtended(x,w,l,c))
      println(c.map(_.min).mkString(","))
    }
    println("Extended ======>nbsol="+nbsol)
    println("obj=="+best)
    cp.printStats
    
    best = 0
    nbsol = 0
  //  cp.objective(l(m)).relax()
    cp.objective.objs(0).best = Int.MinValue 
    cp.runSubjectTo() {
      cp.add(new BinPacking(x, w, l))
      cp.add(new BinPackingFlow(x,w,l,c))
      println(c.map(_.min).mkString(","))
    }
    println("Current ======>nbsol="+nbsol)
    println("obj=="+best)
    cp.printStats
    
    /*
    best = 0
    nbsol = 0
  //  cp.objective(l(m)).relax()
    cp.objective.objs(0).best = Int.MinValue 
    cp.runSubjectTo() {
      cp.add(new BinPacking(x, w, l))
      println(c.map(_.min).mkString(","))
    }
    println("classic ======>nbsol="+nbsol)
    println("obj=="+best)
    cp.printStats    
  */
    
    
  }
  
}
