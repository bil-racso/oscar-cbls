/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cp.premulative


import oscar.cp._
import oscar.algo.reversible._

object RCPSPmaxBench extends App {
  var k = 0;
  for(i <- 57 to 57){
    val (n_orig,b_orig,c_orig) = test("/home/janho/data/rcpspmax/j20/PSP"+i+".SCH",false)
    val (n,b,c) = test("/home/janho/data/rcpspmax/j20/PSP"+i+".SCH",true)
    //print(i+" ")
    //if(b_orig!=b)
      print(i+" ==>\t"+n_orig+"\t"+n+"\t\t"+b_orig+"\t"+b+"\t\t"+c_orig+"\t"+c+"\t")
      if(c && b > b_orig) print("XXX\t") else print("\t")
      if(!c && !c_orig) print("inc  " + (b < b_orig))
      if(c && c_orig) print("comp "+(n < n_orig))
      println()
    //if(n_orig!=n)println(i+" ==>\t"+n_orig+"\t"+n+"\t\t"+b_orig+"\t"+b)
    //if(n_orig!=n && b > 0){println(i+" ====>\t"+n_orig+"\t"+n+"\t\t"+b_orig+"\t"+b); k+=1}
  }
  println(k)
  /*
  val s_orig = System.currentTimeMillis()
  for(i <- 1 to 90){
    val (n_orig,b_orig) = test("/home/janho/data/rcpspmax/ubo20/psp"+i+".sch",false)
    print(i+" ")
  }
  println(System.currentTimeMillis() - s_orig)
  
  val s = System.currentTimeMillis()
  for(i <- 1 to 90){
    val (n,b) = test("/home/janho/data/rcpspmax/ubo20/psp"+i+".sch",true)
    print(i+" ")
  }
  println(System.currentTimeMillis() - s)
  */
  
  def test(instance: String, prop: Boolean): (Int,Int, Boolean) = { 

  val ninf = Int.MinValue
    def computeTransitiveClosure(tab: Array[Array[Int]]){
    //tab = getAdjacencyMatrix();
    val s = tab.length; 
    for(k <- 0 until s){
      for(i <- 0 until s){
        if(tab(i)(k)>ninf){
          for(j <- 0 until s){
            if(tab(k)(j)>ninf && tab(i)(j) < tab(i)(k)+tab(k)(j)){
              tab(i)(j) = tab(i)(k)+tab(k)(j);
            }
          }
        }
      }
    }
  }
  def makeMatrix(ntasks: Int, pred: List[Tuple3[Int,Int,Int]]): Array[Array[Int]] = {
    val tab = Array.tabulate(ntasks)(t => Array.tabulate(ntasks)(s => ninf))
    for((i,j,w) <- pred){
      tab(i)(j) = w
    }
    tab
  }
  val (nTasks, nRes, resourcesCapacities, taskDescriptions, precedences) = RCPmaxReader.readInstance(instance)

  val tab = makeMatrix(nTasks,precedences)
  computeTransitiveClosure(tab)
  
  val durationsData = taskDescriptions.map(_._1)
  val horizon = durationsData.sum + tab(0)(nTasks-1)
  val demandsData = taskDescriptions.map(_._2)
  //val successorsData = taskDescriptions.map(_._3)

  val taskIds = 0 until nTasks
  val resIds = 0 until nRes

  implicit val cp = CPSolver()
  cp.silent=true
  val resourceid = 1
  
  //vars
  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nRes)(r => Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)(r))))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(resourceid))
  val capacities = Array.tabulate(nRes)(r => CPIntVar(resourcesCapacities(r)))

  
  // Constraints	
  // -----------------------------------------------------------------------
  val makespan = ends(nTasks-1)//maximum(ends)

  // Consistency 
  for (t <- taskIds)
    cp.add(ends(t) == starts(t) + durations(t))

  var best = Int.MaxValue 
  
  onSolution {
    best = makespan.min
  }

 

//    val rid = 0;
//  println("digraph{")
//  for (t <- taskIds){
////    if(demands(rid)(t).value>0)
//    println(t+" [label=\""+t+","+durations(t).value+"\"]")
//  }
  //Precedence
//  for ((i,j,w) <- precedences){
//    cp.add(starts(i) + w <= starts(j))
//    println(i+"->"+j+" [label=\""+w+"\"]")
//  }
  try{
  for(i <- taskIds; j <- taskIds){
    val w = tab(i)(j)
    if(w > ninf){
      cp.add(starts(i) + w <= starts(j))
//      if(demands(rid)(i).value>0 && demands(rid)(j).value>0)
//      println(i+"->"+j+" [label=\""+w+"\"]")
    }
  }
//  println("}")
  
  
  // Cumulative
  for (r <- resIds) {
    add(maxCumulativeResource(starts, durations, ends, demands(r), resources, capacities(r), resourceid), Medium)
//    println(capacities(r)+" "+taskIds.filterNot(demands(r)(_).value==0).map(v => (v,durations(v).value,demands(r)(v).value)))
    if(prop)add(new CumulativeLinearWithLags(starts, durations, ends, demands(r), resources, capacities(r), resourceid,tab))
  }

    //println(t+"->"+succ) 
  //}
  // Search
  // -----------------------------------------------------------------------

    
    
  minimize(makespan)
  cp.search {
    splitLastConflict(starts)
    //setTimes(starts, durations, ends,-durations(_).min)
  }
  
  var b = 0;
  cp.onSolution{
    //println(makespan+" "+starts.mkString(","))
    b = makespan.value
  }
  val stat = start(timeLimit=10)
  //println(stat)
  //println(b)
  (stat.nFails,b,stat.completed)
  }catch{
    case e:NoSolutionException =>
      //println(e)
      (1,0,true)
  }
  }
}
