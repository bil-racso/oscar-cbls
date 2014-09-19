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
 * @author Leonard Debroux
 */
package oscar.flatzinc.cp

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











/*
         array_bool_and   

         array_bool_element   

         array_bool_or   


         array_int_element   
      

         array_var_bool_element   
      
         array_var_int_element   
      
         bool2int   
       
         bool_and   
     
         bool_eq   
       
         bool_eq_reif   
        
         bool_le   
       
         bool_le_reif   

         bool_lt   
 
         bool_lt_reif   

         bool_not   

         bool_or   

         bool_xor   

         bool_lin_eq   

         bool_lin_le   


         int_abs   

         int_eq   

         int_eq_reif   
 
         int_le   
 
         int_le_reif   

         int_lt   

         int_lt_reif   

         int_max   

         int_min   
         int_ne   
         int_ne_reif   
         int_plus   
         int_times   
   
         int_lin_ne   

         int_lin_ne_reif   

         int_lin_eq   

         int_lin_eq_reif   

         int_lin_le   

         int_lin_le_reif   


         set_card   

         set_diff   

         set_eq   
   
         set_in
   

         oscar_alldiff   

         alldiff_0   
         all_disjoint   
         oscar_all_equal_int   

         oscar_among   
         oscar_at_least_int   
         oscar_at most_int   
         at_most1   
         oscar_bin_packing   

         oscar_bin_packing_capa   

         oscar_bin_packing_load   

         oscar_circuit   
 
         oscar_count_eq   
 
         oscar_count_geq   
 
         oscar_count_gt   

         oscar_count_leq   

         oscar_count_lt   
    
         oscar_count_neq   
      
         oscar_cumulative

         oscar_decreasing_int   

         oscar_diffn   
     
         oscar_disjoint   

         oscar_distribute   

         oscar_element_bool   
         oscar_element_int   
         exactly_int    //not used, done with among

         oscar_global_cardinality   


         oscar_global_cardinality_closed   

         oscar_global_cardinality_low_up   

         oscar_global_cardinality_low_up_closed   
 
         oscar_increasing_int   

         oscar_int_set_channel   

         oscar_inverse   
         oscar_inverse_set   
         lex_greater_int    //not used, done with lex_less
         lex_greatereq_int    //not used, done with lex_lesseq
         oscar_lex_less_int   
         oscar_lex2    //2D -> 1D done, need to parse the constraint

         oscar_link_set_to_booleans   

         oscar_maximum_int   

         oscar_member_int   

         oscar_minimum_int   

         oscar_nvalue   

         oscar_partition_set   

         oscar_range   

         oscar_regular    //2D -> 1D done

         oscar_roots   

         oscar_sliding_sum   

         oscar_sort   

         oscar_strict_lex2   

         oscar_subcircuit   
 
         oscar_sum_pred   

         oscar_table_int    //2D -> 1D done

         oscar_value_precede_int   
 
         oscar_value_precede_chain_int   

*/

  
