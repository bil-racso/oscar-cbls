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

object RCPSPmax extends App {

  val ninf = Int.MinValue
  
  val (nTasks, nRes, resourcesCapacities, taskDescriptions, precedences) = RCPmaxReader.readInstance("data/rcpsp-max/ubo100/psp4.sch")
  val taskIds = 0 until nTasks
  val resIds = 0 until nRes
  
  val durationsData = taskDescriptions.map(_._1)
  val demandsData = taskDescriptions.map(_._2)

  val tab = GraphAlgorithms.makeMatrix(nTasks,precedences)  
  GraphAlgorithms.todot(tab,durationsData.zip(demandsData.map(_(2)).zip(demandsData.map(_(4)))))
  GraphAlgorithms.computeTransitiveClosure(tab)
      
  val horizon = (0 until nTasks).map(i => durationsData(i).max(tab(i).max)).sum
  

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

  
  var changed = false
  do{
    changed = false
    for(i <- taskIds; j <- i+1 until nTasks){
      if(resIds.exists(r => demands(r)(i).value+demands(r)(j).value > capacities(r).value)){
        if(-durations(j).value < tab(i)(j) && durations(i).value > tab(i)(j)){
          changed = true
          tab(i)(j) = math.max(tab(i)(j),durations(i).value)
        }else if(-durations(i).value < tab(j)(i)  && durations(j).value > tab(j)(i)){
          changed = true
          tab(j)(i) = math.max(tab(j)(i),durations(j).value)
        }else{
          //cp.add(new BinaryDisjunctiveWithTransitionTimes(starts(i),ends(i),starts(j),ends(j),0,0));  
        }      
      }
    }
    GraphAlgorithms.computeTransitiveClosure(tab)
    if((taskIds).exists(i => tab(i)(i) > 0))changed = false

  }while(changed)
    /*
    println((taskIds).map(i => tab(i)(i)))
    val red = GraphAlgorithms.copy(tab)
    GraphAlgorithms.makeTransitiveReduction(red)
  GraphAlgorithms.todot(red,durationsData.zip(demandsData.map(_(2)).zip(demandsData.map(_(4)))))
  */
    
    val cliques = Array.tabulate(tab.length)(t => Array.tabulate(tab.length)(s => 0))
    println("graph{")
    for(i <- taskIds; j <- i+1 until nTasks){
      if(resIds.exists(r => demands(r)(i).value+demands(r)(j).value > capacities(r).value)
          || tab(i)(j) >= durationsData(i) || tab(j)(i) >= durationsData(j)
          ){
        cliques(i)(j) = 1
        cliques(j)(i) = 1
        println(i+"--"+j)
      }
    }
    println("}")
    val cliques2 = GraphAlgorithms.findCliques(cliques)
    cliques2.foreach(println)
    //GraphAlgorithms.todot(red,durationsData)
//    val rid = 0;
  
  for(i <- taskIds; j <- taskIds){
    val w = tab(i)(j)
    if(w > ninf){
      cp.add(starts(i) + w <= starts(j))
    }
  }
  for(c <- cliques2){
    add(unaryResource(c.map(starts(_)).toArray, c.map(durations(_)).toArray,c.map(ends(_)).toArray))
    //add(new Prejunctive(c.map(starts(_)).toArray, c.map(durations(_)).toArray,c.map(ends(_)).toArray,c.map(i => c.map(j => tab(i)(j)).toArray).toArray))
  }
  
  val sol = Array(0,137,74,12,184,0,41,57,16,125,183,84,37,134,146,133,159,105,205,196,114,168,100,114,172,186,133,180,132,159,107,65,24,28,87,153,2,172,47,187,76,187,129,188,196,188,124,220,214,229,204,231)
  for(i <- 0 to 0){
    add(starts(i) == sol(i))
  }
//for((i,t) <- List((1,10),(2,0),(3,0),(4,15),(6,75),(9,28),(10,126),(12,43),(17,232),(21,100),(23,68),(24,71),(27,101),(28,18),
//      (29,25),(36,37),(38,8),(39,229),(45,80),(46,12),(51,105),(59,76),(62,94),(63,80),(64,12),(65,204),(68,71),(70,223),(71,48),
//      (72,219),(73,104),(78,201),(80,87),(81,26),(85,1),(91,94),(96,28),(97,109),(99,85))){
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
    add(new CumulativeLinearWithLags(starts, durations, ends, demands(r), resources, capacities(r), resourceid,tab,true,true))
    //System.exit(0)
  }
//    println(starts.mkString(", "))
//    println(ends.mkString(", "))
  
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

import scala.io.Source

/*
 * Reader used for .rcp instances 
 */

object RCPmaxReader {

  
  def readInstance(dataFile: String) = {

    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")

    //first line : nbAct and nbRes
    val firstLineNumbers = lines.head.split("\t")
    val (nbAct, nbRes) = (firstLineNumbers(0).toInt + 2, firstLineNumbers(1).toInt) 
    val (actIndices, resIndices) = (0 until nbAct, 0 until nbRes)

    lines = lines.drop(1)

    var precedences = List.empty[Tuple3[Int,Int,Int]];
    
    
    for (i <- actIndices) {
  //    println(lines.head)
      val lineNumbers = lines.head.split("\t")
      assert(lineNumbers(0).toInt == i)
      assert(lineNumbers(1).toInt == 1)
      val nbpred = lineNumbers(2).toInt
      val successors = lineNumbers.drop(3).take(nbpred).map(_.toInt)
      val weights = lineNumbers.drop(3+nbpred).map(c => c.substring(1, c.size-1).toInt)
      for(j <- 0 until nbpred){
        precedences = (i,successors(j),weights(j)) :: precedences
      }
      lines = lines.drop(1)
    }
//    println("NEXT")
    val activityDescriptions = new Array[Tuple2[Int, Array[Int]]](nbAct)
    for (i <- actIndices) {
      //println(lines.head)
      val lineNumbers = lines.head.split("\t")

      val requirementArray = new Array[Int](nbRes)
      for (r <- resIndices)
        requirementArray(r) = lineNumbers(3 + r).toInt
      //(dur (requirementArray) (successorArray)
      activityDescriptions(i) = Tuple2(lineNumbers(2).toInt, requirementArray)

      //next line
      lines = lines.drop(1)
    }
    
    val secondLineNumbers = lines.head.split("\t")
    val resourcesCapacities = new Array[Int](nbRes)
    for (i <- resIndices)
      resourcesCapacities(i) = secondLineNumbers(i).toInt

    lines = lines.drop(1)

    assert(lines.isEmpty)
    (nbAct, nbRes, resourcesCapacities, activityDescriptions, precedences)
  }

}

