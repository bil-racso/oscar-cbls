/*******************************************************************************
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
  ******************************************************************************/
/**
 * @author Jean-Noel Monette
 */
package oscar.flatzinc.cp

import oscar.cp._
import oscar.flatzinc.Options
import oscar.flatzinc.parser.FZParser
import scala.collection.mutable.{Map => MMap}
import oscar.flatzinc.model._


class FZCPSolver {
  
  def solve(opts: Options){
    val log = opts.log();
    log("start")
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, false).problem;
     
    log("Parsed.")
    implicit val solver: CPSolver = CPSolver()  
    
    //TODO: Find binary constraints that can be used for views.
    
    val dicoVars = MMap.empty[String,CPIntVar]
    def getVar(v:Variable):CPIntVar = {
      dicoVars.get(v.id) match {
        case None if v.isBound =>
            val c = v match{
              case v:IntegerVariable => CPIntVar(v.value);
              case v:BooleanVariable => CPBoolVar(v.boolValue);
            }
            dicoVars += v.id -> c;
            c
        case Some(c) => c;
      }
    }
    for(v <- model.variables){
      dicoVars(v.id) = v match{
        case bv:BooleanVariable => CPBoolVar()
        case iv:IntegerVariable => iv.domain match{
          case DomainRange(min, max) => CPIntVar(min to max)
          case DomainSet(v) => CPIntVar(v)
          case _ => throw new RuntimeException("unknown domain")
        }
      }
    }
    log("created variables")
    //TODO: Add a try catch for if the problem fails at the root.
    //TODO: Put all the added cstrs in a ArrayBuffer and then post them all at once.
    for(c <- model.constraints){
      //TODO: Take consistency annotation to post constraints.
      c match{
        //TODO: We create variables in cumulative but we might want to memoize those as well!
        //TODO: Actually to avoid creating new variables, we could rewrite the cumulative in the minizinc definition to create those variables while flattening, and CSE will take care of it.
        case cumulative(s,d,r,capa,_) => solver.add(oscar.cp.maxCumulativeResource(s.map(getVar), d.map(getVar), s.zip(d).map(vv => getVar(vv._1)+getVar(vv._2)), r.map(getVar), getVar(capa)))
        case maximum_int(x,y,_) => 
          solver.add(oscar.cp.maximum(y.map(getVar), getVar(x)))
          solver.add(y.map(v => getVar(v) <= getVar(x)))
        //TODO: Handle binary and ternary cases, as well as all unit weights
        case int_lin_eq(x,y,z,_) => solver.add(oscar.cp.weightedSum(x.map(_.value), y.map(getVar), getVar(z)))
        case all_different_int(x,_) => solver.add(oscar.cp.allDifferent(x.map(getVar)), Medium)//Weak, Strong, Medium
      }
    }
    log("constraints posted")
    model.search.obj match{
     case Objective.SATISFY => 
     case Objective.MAXIMIZE => maximize(getVar(model.search.variable.get))
     case Objective.MINIMIZE => minimize(getVar(model.search.variable.get))
    }
    log("objective created")
    def handleSolution() = {
     model.solution.handleSolution(
      (s: String) => dicoVars.get(s) match {
        case Some(intVar) =>
          intVar.value + ""
        case r => if(s=="true" || s=="false") s 
        else try{
          s.toInt.toString()
        }catch{
          case e: NumberFormatException => {
            throw new Exception("Unhappy: "+r+ " "+s)
          }
        }
     });
    }
    solver.onSolution{
      //println("found")
      handleSolution()
    }
    solver.silent = true
    //TODO: Take into account the search annotations
    solver.search(binaryFirstFail((model.variables-model.search.variable.get).map(getVar).toArray[CPIntVar]))
    log("search created")
    //TODO: search for more than one solution for optimisation
    //TODO: Remove the time spent in parsing and posting
    val stats = solver.start(nSols= Int.MaxValue,timeLimit=60*15)
    log(stats.toString)

    
  }

  
}
  
