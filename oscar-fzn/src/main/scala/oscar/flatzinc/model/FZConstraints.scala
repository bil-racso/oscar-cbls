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
package oscar.flatzinc.model


abstract class Constraint(val variables: Array[Variable],val annotations: List[Annotation]) {
  for(v <- variables){
    v.addConstraint(this);
  }
  //Can be posted as an invariant
  def canDefineVar = false
  //The variables appearing in the constraint
  def getVariables():Array[Variable] = variables
  
  //The variables of the constraint which it can functionally define 
  //TODO generalize to arrays of defined variables (e.g. in sort/bin-packing)
  def getCandidateDefVars():Array[Variable] = Array.empty[Variable]
  //def simplify(p: FZProblem){}
  //The variable the constraint functionally defines
  //TODO: generalize to arrays of defined variables (e.g. in sort/bin-packing)
  def setDefinedVar(v: Variable) = {
    definedVars match {
      case None => v.definingConstraint = Some(this);
                    definedVars = Some(v);
      case Some(vv) => throw new Exception("Not supported yet.")
    }
  }
  def unsetDefinedVar(v: Variable) = {
    definedVars = definedVars match {
      case Some(vv) if v==vv => {
        v.definingConstraint = None
        None  
      }
      case _ => throw new Exception(v+" was not defined by "+this)
    }
  }
  private var definedVars = Option.empty[Variable]
  def definedVar = definedVars
  
  //True if the constraints annotations says that it defines x
  //def definesVar(x: Variable):Boolean = {annotations.foldLeft(false)((acc,y)=> (y.name == "defines_var" && y.args(0).asInstanceOf[ConcreteVariable].id == x.id ) || acc)}
  
  //this must be called when a constraint is removed from the model to remove references from variables. 
  def retract(){
    //todo remove the defined var if any
    for(v <- variables){
      v.removeConstraint(this)
    }
  }
}

abstract class SimpleDefiningConstraint(variables: Array[Variable], val maybeDefinedVar: Variable, ann:List[Annotation])
  extends Constraint(variables, ann){
  override def canDefineVar = true
  override def getCandidateDefVars() = Array(maybeDefinedVar)
}
abstract class ReifiedConstraint(variables: Array[Variable], r: Variable, ann:List[Annotation]) extends SimpleDefiningConstraint(variables++Array(r),r,ann) {

}

case class reif(val c: Constraint,r: Variable) extends ReifiedConstraint(c.variables,r,c.annotations){
  c.retract()
}

abstract class AllDefiningConstraint(variables: Array[Variable], ann:List[Annotation])
  extends Constraint(variables, ann){
  override def canDefineVar = true
  override def getCandidateDefVars() = variables
}

// ----------------------------------
case class array_bool_and(as: Array[Variable], r: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(as++Array(r),r,ann);

//Here, the "as" are NOT Variables, but Constants!
case class array_bool_element(b: Variable, as: Array[Variable], c: Variable, ann: List[Annotation]) 
  extends SimpleDefiningConstraint(Array(b,c),c,ann);
  
case class array_bool_or(as: Array[Variable], r: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(as++Array(r),r,ann);

case class array_bool_xor(as: Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(as,ann)

case class array_int_element(b: Variable, as: Array[Variable], c: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(b,c),c,ann);

case class array_var_bool_element(b: Variable, as: Array[Variable], c: Variable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(as++Array(b,c),c,ann);

case class array_var_int_element(b: Variable, as: Array[Variable], c: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(as++Array(b,c),c,ann);

case class bool2int(b: Variable, x: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(b,x),ann)

case class bool_and(a: Variable, b: Variable, r: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,r),r,ann);
  
case class bool_clause(a: Array[Variable], b: Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(a++b,ann)

case class bool_eq(x: Variable, y: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y),ann)

case class bool_le(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class bool_lin_eq(params:Array[Variable],vars:Array[Variable], sum:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(vars,ann)

case class bool_lin_le(params:Array[Variable],vars:Array[Variable], sum:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars,ann)

case class bool_lt(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class bool_not(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation])
  extends AllDefiningConstraint(Array(a,b),ann)

case class bool_or(a: Variable, b: Variable, r: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends ReifiedConstraint(Array(a,b),r,ann)

case class bool_xor(a: Variable, b: Variable, r: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends ReifiedConstraint(Array(a,b),r,ann)

case class int_abs(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b),b,ann)

case class int_div(a: Variable, b: Variable, c:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)
  
case class int_eq(x: Variable, y: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y),ann)

case class int_le(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class int_lin_eq(params:Array[Variable],vars:Array[Variable], sum:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars,ann){
  override def canDefineVar = true
  override def getCandidateDefVars():Array[Variable]  = {
    return vars.zip(params).filter((t) => Math.abs(t._2.min) == 1).map(_._1)
  }
}

case class int_lin_le(params:Array[Variable],vars:Array[Variable], sum:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars,ann)

case class int_lin_ne(params:Array[Variable],vars:Array[Variable], sum:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars,ann)

case class int_lt(a: Variable, b: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class int_min(a: Variable, b: Variable, c:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_max(a: Variable, b: Variable, c:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_mod(a: Variable, b: Variable, c:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_ne(x: Variable, y: Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(x,y),ann)

case class int_plus(x:Variable,y:Variable,z:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y,z),ann)

case class int_times(x:Variable,y:Variable,z:Variable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(Array(x,y,z),z,ann) 

case class set_in(x:Variable,s:Domain, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(x),ann)


case class all_different_int(xs:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(xs,ann)


case class at_least_int(n:Variable,x:Array[Variable],v:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)
case class at_most_int(n:Variable,x:Array[Variable],v:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)
case class exactly_int(n:Variable,x:Array[Variable],v:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)
case class among(n:Variable,x:Array[Variable],v:Domain, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(x++Array(n),n,ann)
case class count_eq(xs:Array[Variable], y:Variable, cnt:Variable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(xs++Array(y,cnt),cnt,ann)


case class circuit(x:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)
case class subcircuit(x:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)

case class bin_packing_capa(c:Array[Variable],bin:Array[Variable],w:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(bin,ann)
case class bin_packing(c:Variable,bin:Array[Variable],w:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(bin,ann)
//TODO: defines the loads
case class bin_packing_load(load:Array[Variable],bin:Array[Variable],w:Array[Variable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(load++bin,ann)

case class cumulative(s:Array[Variable], d:Array[Variable],r:Array[Variable],b:Variable,ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(s++d++r++Array(b),ann)

case class diffn(x:Array[Variable], y:Array[Variable],dx:Array[Variable],dy:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++y++dx++dy,ann)

//TODO: defines card
case class distribute(card:Array[Variable], value:Array[Variable],base:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(card++value++base,ann)
//TODO: defines the counts
case class global_cardinality_closed(x:Array[Variable], cover:Array[Variable],count:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++count,ann)
//TODO: defines the counts
case class global_cardinality(x:Array[Variable], cover:Array[Variable],count:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++count,ann)
case class global_cardinality_low_up(x:Array[Variable], cover:Array[Variable],lbound:Array[Variable],ubound:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)
case class global_cardinality_low_up_closed(x:Array[Variable], cover:Array[Variable],lbound:Array[Variable],ubound:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x,ann)

//TODO: defines f or invf
case class inverse(f:Array[Variable], invf:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(f++invf,ann)

case class maximum_int(m:Variable, x:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(m)++x,m,ann)
case class minimum_int(m:Variable, x:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(m)++x,m,ann)
case class member_int(x:Array[Variable],y:Variable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(y)++x,ann)

case class sort(x:Array[Variable], y:Array[Variable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++y,ann)
