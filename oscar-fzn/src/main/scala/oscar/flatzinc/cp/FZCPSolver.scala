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

import oscar.cp.modeling.CPSolver
import oscar.flatzinc.Options
import oscar.flatzinc.parser.FZParser
import oscar.cp.core.CPIntVar
import scala.collection.mutable.{Map => MMap}
import oscar.cp.core.CPBoolVar
import oscar.flatzinc.model._
import oscar.cp.modeling.Constraints

/*
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPIntVar
import scala.collection.mutable.{Map => MMap}
import oscar.cp.core.CPBoolVar
import scala.Array.canBuildFrom
import scala.collection.mutable.{Map => MMap}
import oscar.flatzinc.model._
import oscar.cp.constraints.And
import oscar.cp.modeling._
import oscar.cp.constraints.ElementVar
*/
//NOTE by JNM: Commented to avoid dependency on the CP part.
class FZCPSolverProblem {
  
  def solve(opts: Options){
    val log = opts.log();
    log("start")
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, false).problem;
     
    log("Parsed.")
    val solver: CPSolver = CPSolver()  
    
    //TODO: Find binary constraints that can be used for views.
    
    val dicoVars = MMap.empty[String,CPIntVar]
    for(v <- model.variables){
      dicoVars(v.id) = v match{
        case bv:BooleanVariable => CPBoolVar()(solver)
        case iv:IntegerVariable => iv.domain match{
          case DomainRange(min, max) => CPIntVar(min to max)(solver)
          case DomainSet(v) => CPIntVar(v)(solver)
          case _ => throw new RuntimeException("unknown domain")
        }
      }
    }
    for(c <- model.constraints){
      c match{
        case cumulative(s,d,r,capa,_) => //add oscar.cp.cumulative
        case maximum_int(x,y,_) => //add oscar.cp.maximum
        case int_lin_eq(x,y,z,_) => // add a sum. Need to specialise?
      }
    }
  }
  
/*
  def instanciate(vari: Variable, cp: CPSolver, varMap: MMap[Variable, CPIntVar]) {
    vari match {
      case ConcreteVariable(i: String, dom: Domain,ann) =>
        if (varMap.contains(vari)) return
        else {
          val variable: CPIntVar = dom match {
            case DomainRange(min, max) => CPIntVar(min to max)(cp)
            case DomainSet(v) => CPIntVar(v)(cp)
            case _ => throw new RuntimeException("unknown domain")
          }
          varMap += (vari -> variable)
        }
 /*     case OffSetVariable(i, offset, x,ann) =>
        if (varMap.contains(vari)) return
        else {
          instanciate(x, cp, varMap)
          assert(varMap.contains(x))
          varMap += (vari -> (varMap(x) + offset))
        }
      case MinusVariable(i, x,ann) =>
        if (varMap.contains(vari)) return
        else {
          instanciate(x, cp, varMap)
          assert(varMap.contains(x))
          varMap += (vari -> (-varMap(x)))
        }*/
    }
  }
  
  
  def instanciate(cp: CPSolver, prob: FZProblem): Map[String,CPIntVar] = {
    var res = Map.empty[String,CPIntVar]
    val varMap = MMap[Variable,CPIntVar]()
    for ((s,v) <- prob.map){
      instanciate(v,cp, varMap)
      res += s -> varMap(v)
    }
    res
  }
  
  
  def search = {
    
  }
  
  def addConstraints(cp: CPSolver, prob: FZProblem, varMap: Map[String,CPIntVar]) = {
    for (c <- prob.constraints) {
    c match {
        case array_bool_and(as,r,ann) => 
        	val x = as.map(y => new CPBoolVar(varMap(y.id)))
        	val y = new CPBoolVar(varMap(r.id))
        	cp.add(new And(x,y))          
//        case array_bool_element(b,as,c,ann) =>
//        	val x = as.map(y => new CPBoolVar(varMap(y.id)))
//        	val y = new CPBoolVar(varMap(b.id))
//        	val z = new CPBoolVar(varMap(c.id))
//        	cp.add(new ElementVar(x,y,z))  
        case bool_eq(x,y,ann) => throw new RuntimeException("should not be posted")
        case bool2int(b,x,ann) => throw new RuntimeException("should not be posted")
        case int_eq(x,y,ann) => throw new RuntimeException("should not be posted")
       
      }
    }
  }
  
*/
}
  
