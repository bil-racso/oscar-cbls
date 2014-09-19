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
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.transfo

import scala.util.control.Breaks._
import oscar.flatzinc.model._

object FZModelTransfo {
  def findInvariants(implicit model: FZProblem) = {
    //Find all free variables of the model.
    var freeVariables: List[Variable] =
      model.map.filterKeys((id: String) =>
        model.map(id).id == id  //to take each variable only once (as there may be aliases)
        && !model.map(id).isDefined //to remove the ones already defined
        && model.map(id).min != model.map(id).max //to remove de facto constants
          //JNM: Removed the following because the CP search variables are maybe not the CBLS search variables. 
          //This makes a huge difference for the BACP for instance.
          //&&
          //!model.search.heuristics.exists((h) => h._1.exists((v: Variable) => v == model.map(id) && (v.max - v.min) < 1000)) // I am not sure if this is needed...
          ).values.toList
    
          
    //Remove free variables which are defined by a eq constraint with a constant argument. 
    //Replaced this code by checking whether the domain is a singleton. above (after propagation of the domains)
    
          
    //Find all constraints which do not already define a variable and can define a variable (be turned into an invariant)
    def freeC(cstrs: List[Constraint]): List[Constraint] = {
      cstrs.filter((c: Constraint) => c.definedVar == None && c.canDefineVar)
    }
    //For all free variables
    for (v <- freeVariables.sortWith((x: Variable, y: Variable) => x.max - x.min > y.max - y.min)) {
      breakable { //Select the first suitable constraint
        if(freeC(v.cstrs).filter(c=>c.getCandidateDefVars().contains(v) && ! dependsOn(c,v,false)).length > 1){
          println("%!! Found a variable that could be defined by more than one invariant:"+v+" "+(freeC(v.cstrs).filter(c=>c.getCandidateDefVars().contains(v) && ! dependsOn(c,v,false)).toString))
        }
        for (c <- freeC(v.cstrs)) {//In any order...//TODO: Is there any interesting order?
          if (c.getCandidateDefVars().contains(v)) {
            if (!dependsOn(c,v,false)) {
              v.isDefined = true;
              c.definedVar = Some(v);
              break;
            }
          }
        }
      }
    }
  }
  def dependsOn(c: Constraint, v: Variable,test: Boolean = true): Boolean = {
    c.getVariables().exists(w => if (w.isDefined) c.definedVar!= Some(w) && dependsOn(w.cstrs.find(c=> c.definedVar == Some(w)).get,v) 
                                 else test && w == v)
  }

  
  
  
  
  def propagateDomainBounds(model: FZProblem) = {
    //TODO: Also reduce the domains from GCC_closed and other such constraints. Or do it from the minizinc to flatzinc level
    //TODO: Do more than just the bounds, then handle the in_set constraint here.
    model.constraints = model.constraints.filter(c => 
      c match {
        case int_le(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value); false
        case int_le(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value); false
        case int_lt(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value+1); false
        case int_lt(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value-1); false
        case bool_le(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value); false
        case bool_le(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value); false
        case bool_lt(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value+1); false
        case bool_lt(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value-1); false
        case int_eq(x: ConcreteConstant, y: ConcreteVariable, ann) => y.dom.geq(x.value); y.dom.leq(x.value); false
        case int_eq(x: ConcreteVariable, y: ConcreteConstant, ann) => x.dom.geq(y.value); x.dom.leq(y.value); false
        case bool_eq(x: ConcreteConstant, y: ConcreteVariable, ann) => y.dom.geq(x.value); y.dom.leq(x.value); false
        case bool_eq(x: ConcreteVariable, y: ConcreteConstant, ann) => x.dom.geq(y.value); x.dom.leq(y.value); false
        //The cstrs below might need to be iterated until fixpoint...
        case int_le(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); x.dom.leq(y.max); true
        case int_lt(x:ConcreteVariable, y:ConcreteVariable, _ ) =>{
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          y.dom.geq(x.min+1); x.dom.leq(y.max-1); 
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          true
        } 
        case int_eq(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); y.dom.leq(x.max); x.dom.geq(y.min); x.dom.leq(y.max); true
        case bool_eq(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); y.dom.leq(x.max); x.dom.geq(y.min); x.dom.leq(y.max); true
        case _ => true 
      })
  }
}