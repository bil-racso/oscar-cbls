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
import oscar.cp.scheduling.constraints.BinaryDisjunctiveWithTransitionTimes

object RCPSPmaxBench extends App {
  var k = 0;
  val lst = List(("ubo10/psp",".sch",90),("ubo20/psp",".sch",90),("ubo50/psp",".sch",90),("ubo100/psp",".sch",90),("j10/PSP",".SCH",270),("j20/PSP",".SCH",270),("j30/PSP",".SCH",270))
  val bench = 2
  for(i <- 4 to lst(bench)._3){
    print(i+" ==>\t")
    val (n_orig,b_orig,c_orig) = test("data/rcpsp-max/"+lst(bench)._1+i+lst(bench)._2,false,false,true)
    print(n_orig+"\t")
    val (n,b,c) = test("data/rcpsp-max/"+lst(bench)._1+i+lst(bench)._2,false,true,true,true,true)
//    val nbc1 = test("data/rcpsp-max/"+lst(bench)._1+i+lst(bench)._2,true,true,false)
//    val nbc2 = test("data/rcpsp-max/"+lst(bench)._1+i+lst(bench)._2,true,false,true)
//    val nbc3 = test("data/rcpsp-max/"+lst(bench)._1+i+lst(bench)._2,true,false,false)
    //print(i+" ")
    //if(b_orig!=b)
      
      print(n+"\t\t"+b_orig+"\t"+b+"\t\t"+c_orig+"\t"+c+"\t")
      if(c && (b > b_orig || (b == 0 && b_orig > 0))) print("XXX\t") else print("\t")
      if(!c && !c_orig) print("inc  " + (b < b_orig && b > 0))
      if(c && c_orig) print("comp "+(n < n_orig))
      println()
//      println((n_orig,b_orig,c_orig)+"\t"+(n,b,c)+"\t"+nbc1+"\t"+nbc2+"\t"+nbc3)
    //if(n_orig!=n)println(i+" ==>\t"+n_orig+"\t"+n+"\t\t"+b_orig+"\t"+b)
    if(n_orig!=n && b > 0 && c && c_orig){/*println(i+" ====>\t"+n_orig+"\t"+n+"\t\t"+b_orig+"\t"+b);*/ k+=1}
  }
  println(k)
  /*
  val s_orig = System.currentTimeMillis()
  for(i <- 1 to 90){
    val (n_orig,b_orig,c_orig) = test("/home/janho/data/rcpspmax/ubo20/psp"+i+".sch",false)
    print(i+" ")
  }
  println(System.currentTimeMillis() - s_orig)
  
  val s = System.currentTimeMillis()
  for(i <- 1 to 90){
    val (n,b,c) = test("/home/janho/data/rcpspmax/ubo20/psp"+i+".sch",true)
    print(i+" ")
  }
  println(System.currentTimeMillis() - s)
  */
  
  def test(instance: String, prop: Boolean,useprejunctive: Boolean,preprodisj: Boolean,pest:Boolean = true,plst: Boolean = true): (Int,Int, Boolean) = { 

  val ninf = Int.MinValue
  
  val (nTasks, nRes, resourcesCapacities, taskDescriptions, precedences) = RCPmaxReader.readInstance(instance)
  val taskIds = 0 until nTasks
  val resIds = 0 until nRes

  val durationsData = taskDescriptions.map(_._1)
  val demandsData = taskDescriptions.map(_._2)
  
  val tab = GraphAlgorithms.makeMatrix(nTasks,precedences)
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
      if((taskIds).exists(i => tab(i)(i) > 0))changed = false//this is not feasible
    }while(changed)
  
  try{
   
  
  for(i <- taskIds; j <- taskIds){
    val w = tab(i)(j)
    if(w > ninf){
      cp.add(starts(i) + w <= starts(j))
    }
  }
   if(preprodisj){
      val cliques = Array.tabulate(tab.length)(t => Array.tabulate(tab.length)(s => 0))
      for(i <- taskIds; j <- i+1 until nTasks){
        if(resIds.exists(r => demands(r)(i).value+demands(r)(j).value > capacities(r).value) 
            ||( tab(i)(j) >= durationsData(i) || tab(j)(i) >= durationsData(j))){
          cliques(i)(j) = 1
          cliques(j)(i) = 1
        }
      }
      //println("XXX")
      val cliques2 = GraphAlgorithms.findCliques(cliques)
      //cliques2.foreach(println) 
      for(c <- cliques2){
        add(unaryResource(c.map(starts(_)).toArray, c.map(durations(_)).toArray,c.map(ends(_)).toArray))
        if(useprejunctive){
          add(new Prejunctive(c.map(starts(_)).toArray, c.map(durations(_)).toArray,c.map(ends(_)).toArray,c.map(i => c.map(j => tab(i)(j)).toArray).toArray))
        }
      }
   }
  
  // Cumulative
  for (r <- resIds) {
    add(maxCumulativeResource(starts, durations, ends, demands(r), resources, capacities(r), resourceid), Medium)
//    println(capacities(r)+" "+taskIds.filterNot(demands(r)(_).value==0).map(v => (v,durations(v).value,demands(r)(v).value)))
    if(prop)add(new CumulativeLinearWithLags(starts, durations, ends, demands(r), resources, capacities(r), resourceid,tab,pest,plst))
  }

    //println(t+"->"+succ) 
  //}
  // Search
  // -----------------------------------------------------------------------

    
    
  minimize(makespan)
  cp.search {
    splitLastConflict(starts)
    //binaryStatic(starts)
    //setTimes(starts, durations, ends,-durations(_).min)
  }
  
  var b = 0;
  cp.onSolution{
    //println(makespan+" "+starts.mkString(","))
    b = makespan.value
  }
  /*
  for(k <- makespan.min to makespan.max if b==0){
    println(k)
    val stat = startSubjectTo(1,timeLimit=20){
      add(makespan <= k)
    }
    if(!stat.completed){
      println("gave up")
      b+=1
    }
  }*/
  val stat = start(timeLimit=20)
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
