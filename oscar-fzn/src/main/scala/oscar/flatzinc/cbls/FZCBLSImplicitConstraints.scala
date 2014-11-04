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
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.flatzinc.cbls.support._
import oscar.flatzinc.model._
import scala.collection.mutable.ArrayOps


    
class FZCBLSImplicitConstraints(val cblsmodel:FZCBLSModel) {
  
  def findAndPostImplicit(constraints: List[Constraint]) = {
      //TODO: DO not like the filtering here.
      constraints.sortBy(c => - c.variables.length).partition((constraint: Constraint) =>
        constraint match {
          case all_different_int(xs, ann) => tryAllDiff(xs)
          case circuit(xs, ann) => tryCircuit(xs)
          case subcircuit(xs, ann) => trySubCircuit(xs)
          case global_cardinality_closed(xs,vals,cnts,ann) => tryGCC(xs,vals,cnts,true)
          //TODO: detect when a GCC is closed even if not declared as such (should come much earlier)
          case global_cardinality(xs,vals,cnts,ann) => tryGCC(xs,vals,cnts,false)
          case global_cardinality_low_up_closed(xs,vals,low,up,ann) => tryGCClu(xs,vals,low,up,true)
          case global_cardinality_low_up(xs,vals,low,up,ann) => tryGCClu(xs,vals,low,up,false)
          case int_lin_eq(coeffs,vars,sum,ann) => trySum(vars,coeffs,sum)
          case bool_lin_eq(coeffs,vars,sum,ann) => trySum(vars,coeffs,sum)
          case _ => false;
        })
  
  }
  
  object EnsureDomain{
      def apply(v: CBLSIntVarDom) = {
        val t = new ValueTracker(v,cblsmodel.c)
        t.update(true)
      }
    }
    object EnsureDomainRelax{
      def apply(v: CBLSIntVarDom,newMin:Int,newMax:Int) = {
        val t = new ValueTracker(v,cblsmodel.c )
        v.minVal = newMin
        v.maxVal = newMax
        t.update()
      }
    }
    
  def tryAllDiff(xs: Array[Variable]):Boolean = {
      if(allOK(xs)){
        cblsmodel.addNeighbourhood(new AllDifferent(xs.map(cblsmodel.getCBLSVar(_)), cblsmodel.objective(), cblsmodel.c))
        true
      }else false
    }
    def tryCircuit(xs: Array[Variable]):Boolean = {
      if (allOK(xs)){
        //TODO: remove some of the defined if it is better to use the Circuit implicit constraint
        //TODO: We assume that the offset is 1. Is it always the case?
        
        for(i <- 0 until xs.length){
          EnsureDomainRelax(cblsmodel.getCBLSVar(xs(i)),1,xs.length)
        }
        cblsmodel.addNeighbourhood(new ThreeOpt(xs.map(cblsmodel.getCBLSVar(_)),cblsmodel.objective(), cblsmodel.c,1))
        true
      }else{
        false
      }
    }
    def trySubCircuit(xs: Array[Variable]):Boolean = {
      if (allOK(xs)){
        //TODO: We assume that the offset is 1. Is it always the case?
        //TODO: remove some of the defined if it is better to use the Circuit implicit constraint
        for(i <- 0 until xs.length){
          EnsureDomainRelax(cblsmodel.getCBLSVar(xs(i)),1,xs.length)
        }
        cblsmodel.addNeighbourhood(new ThreeOptSub(xs.map(cblsmodel.getCBLSVar(_)),cblsmodel.objective(), cblsmodel.c,1))
        true
      }else{
        println("BOF")
        xs.foreach(x => println(x+ " " +cblsmodel.vars.contains(cblsmodel.getCBLSVar(x))))
        println(cblsmodel.vars)
        false
      }
    }
    
    def tryGCC(xs: Array[Variable],vals: Array[Variable], cnts: Array[Variable],closed: Boolean):Boolean ={
      if (allOK(xs) && cnts.forall(c => c.min==c.max)){//Only for fixed count variables for now
        cblsmodel.addNeighbourhood(new GCCNeighborhood(xs.map(cblsmodel.getCBLSVar(_)),vals.map(_.min),cnts.map(_.min),cnts.map(_.max),closed,cblsmodel.objective(),cblsmodel.c))
        true
      }else{
        false
      }
    }
    def tryGCClu(xs: Array[Variable],vals: Array[Variable], low: Array[Variable],up: Array[Variable],closed: Boolean):Boolean ={
      if (allOK(xs)){
        cblsmodel.addNeighbourhood(new GCCNeighborhood(xs.map(cblsmodel.getCBLSVar(_)),vals.map(_.min),low.map(_.min),up.map(_.max),closed,cblsmodel.objective(),cblsmodel.c))
        true
      }else{
        false
      }
    }
  def trySum(xs: Array[Variable], coeffs: Array[Variable],sum:Variable): Boolean = {
      if (allOK(xs) && coeffs.forall(x => x.min == 1 || x.min == -1)) {
        cblsmodel.addNeighbourhood(new SumNeighborhood(xs.map(cblsmodel.getCBLSVar(_)),coeffs.map(_.min),sum.min,cblsmodel.objective(),cblsmodel.c))
        true
      }else{
        false
      }
    }
  
  def allOK(xs: Array[Variable]):Boolean = {
    xs.forall(x => ! x.isDefined && cblsmodel.vars.contains(cblsmodel.getCBLSVar(x)))
  }
}