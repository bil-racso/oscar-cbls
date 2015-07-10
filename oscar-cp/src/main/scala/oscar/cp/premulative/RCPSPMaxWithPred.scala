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

object RCPSPMaxWithPred extends App {

  val ninf = Int.MinValue
  
  val (nTasks, nRes, resourcesCapacities, taskDescriptions, precedences) = RCPmaxReader.readInstance("data/rcpsp-max/j20/PSP128.SCH")

  val tab = makeMatrix(nTasks,precedences)
  computeTransitiveClosure(tab)
      
  val durationsData = taskDescriptions.map(_._1)
  val horizon = durationsData.sum + math.max(0,tab(0)(nTasks-1))
  val demandsData = taskDescriptions.map(_._2)
  //val successorsData = taskDescriptions.map(_._3)

  val taskIds = 0 until nTasks
  val resIds = 0 until nRes

  implicit val cp = CPSolver()
  //cp.silent=true
  val resourceid = 1
  
  //vars
  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nRes)(r => Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)(r))))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(resourceid))
  val capacities = Array.tabulate(nRes)(r => CPIntVar(resourcesCapacities(r)))

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
  println("digraph{")
  for (t <- taskIds){
//    if(demands(rid)(t).value>0)
    println(t+" [label=\""+t+","+durations(t).value+"\"]")
  }
  //Precedence
//  for ((i,j,w) <- precedences){
//    cp.add(starts(i) + w <= starts(j))
//    println(i+"->"+j+" [label=\""+w+"\"]")
//  }
  for(i <- taskIds; j <- taskIds){
    val w = tab(i)(j)
    if(w > ninf){
      println(i+"->"+j+" [label=\""+w+"\"]")
      cp.add(starts(i) + w <= starts(j))
//      if(demands(rid)(i).value>0 && demands(rid)(j).value>0)
      
    }
  }
  println("}")
//  for((i,t) <- List((2,0),(4,9),(5,0),(6,0),(9,48),(11,21),(12,60),(15,43),(16,34))){
//    add(starts(i)==t)
//  }
//  add(starts(1)==7)  
//  add(starts(2)==39)
//  add(starts(3)==19)
//  add(starts(4)==32)
//  add(starts(5)==0)
//  add(starts(6)==26)
//  add(starts(11)==27)
 // add(starts(12)==40)
 //But task 12 is moved to 44
  
  
  
  // Cumulative
  for (r <- List(0,1,2,3,4)) {
    add(maxCumulativeResource(starts, durations, ends, demands(r), resources, capacities(r), resourceid), Medium)
    println(capacities(r)+" "+taskIds.filterNot(demands(r)(_).value==0).map(v => (v,durations(v).value,demands(r)(v).value)))
 
    add(new Premulative(starts, durations, ends, demands(r), resources, capacities(r), resourceid,tab))

  }
    println(starts.mkString(", "))
    println(ends.mkString(", "))
  
 // add(new CumulativeLinearWithLags(starts, durations, ends, demands(0), resources, capacities(0), resourceid,tab))
  //System.exit(0)
    //println(t+"->"+succ) 
  //}
  // Search
  // -----------------------------------------------------------------------
//add(makespan <=17)
    
    println(starts.mkString(", "))
    println(ends.mkString(", "))
    
  minimize(makespan)
  cp.search {
    splitLastConflict(starts)
    //setTimes(starts, durations, ends,-durations(_).min)
  }
  var b = 0;
  cp.onSolution{
    println(makespan+" "+starts.mkString(","))
    b = makespan.value
  }
  val stat = start()
  println(stat)
  println(b)
}

