/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.constraints.core

import oscar.cbls.invariants.core.computation.{Variable, IntVar, Model}
import oscar.cbls.objective.core.ObjectiveTrait
import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.lib.numeric.{Prod2, Prod, Sum}

/** A constraint system is a composition of constraints.
 * It is itself a constraint, offering the same features, namely, a global violation and a violation specific to each variable.
 * monitoring the violation of a variable requires that the ConstraintSystem has been notified that the variable should have an associated violation degree.
 * This is achieved by calling the method registerForViolation(v:Variable).
 * @param _model is the model in which all the variables referenced by the constraints are declared.
 */
class ConstraintSystem(val _model:Model) extends Constraint with ObjectiveTrait{
  //ConstraintSystems do not act as invariant because everything is subcontracted.

  model = _model

  finishInitialization(_model)

  val Violation:IntVar = new IntVar(this.model,0,Int.MaxValue,0,"Violation")
  private var Violations:SortedMap[Variable,IntVar] = SortedMap.empty[Variable,IntVar]
  private var PostedConstraints:List[(Constraint,IntVar)] = List.empty
  private var AllVars:SortedMap[Variable,List[(Constraint,IntVar)]]=SortedMap.empty

  /**
   * @return the constraints posted in the constraint system, together with their weighting factor.
   */
  def getPostedConstraints:List[(Constraint,IntVar)] = PostedConstraints

  /**Method used to post a constraint in the constraint system.
   * Cannot be called after the constraint system has been closed.
   * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
   * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
   * @param c is the posted constraint.
   * @param weight is the weight that is used in the weighted sum of the violation degrees.
   */
  def post(c:Constraint,weight:IntVar=null){

    assert(c.getPropagationStructure == this.model || c.getPropagationStructure == null, "constraints must be registered to same propagation structure as constraint system")
    PostedConstraints = (c,weight) :: PostedConstraints

    for(variable <- c.getConstrainedVariables){
      val oldConstrAndWeightList = AllVars.getOrElse(variable,List.empty)
      AllVars += ((variable,((c,weight)::oldConstrAndWeightList)))
    }
  }

  /**Must be invoked before the violation can be queried.
   * no constraint can be added after his method has been called.
   * this method must also be called before closing the model.
   */
  def close(){
    Violation <== Sum(PostedConstraints.map((constraintANDintvar) => {
      if(constraintANDintvar._2 == null) constraintANDintvar._1.getViolation
      else Prod(SortedSet(constraintANDintvar._1.getViolation,constraintANDintvar._2)).toIntVar
    }))

    setObjectiveVar(Violation)

    var LocalViolations = SortedMap.empty[Variable,IntVar]
    AllVars.foreach((VariableAndConstrAndWeightList) => {
      val ConstrAndWeightList = VariableAndConstrAndWeightList._2
      val variable = VariableAndConstrAndWeightList._1

      val product:List[IntVar] = ConstrAndWeightList.map((ConstrAndWeight) => {
        val constr = ConstrAndWeight._1
        val weight = ConstrAndWeight._2
        if(weight == null) constr.getViolation(variable)
        else (Prod2(constr.getViolation(variable),weight)).toIntVar
      })
      if (!product.isEmpty && product.tail.isEmpty) {LocalViolations += ((variable,product.head))
      }else{LocalViolations += ((variable,Sum(product).toIntVar))}
    })

    val WatchedVariables = Violations.keySet
    var AccViol:SortedMap[Variable,List[IntVar]] = SortedMap.empty[Variable,List[IntVar]]
    for(VarOflocalviol <- LocalViolations.keys){
      //TODO: this is deeply inefficient: we query GetSourceVariables so many times, and this procedure is not focused
      val sources = model.getSourceVariables(VarOflocalviol).intersect(WatchedVariables)
      for(source <- sources){
        //TODO: no such expensive operations please!!!
        AccViol += ((source,LocalViolations(VarOflocalviol) :: AccViol.getOrElse(source,List.empty)))
      }
    }
    AccViol.foreach(ViolAndElements => Violations(ViolAndElements._1) <== Sum(ViolAndElements._2))
  }

  /**Call this method to notify that the variable should have a violation degree computed for the whole constraint system.
   * it is not compulsory that the variable is directly involved in the constraints. It can be involved indirectly, even through invariants.
   * The variables registered here are the ones and only ones that are considered as constrained by the constraint system, and returned by the method getConstrainedVariables
   * @param v the variable that is registered
   */
  def registerForViolation(v:Variable){
    //TODO: this is again too slow
    Violations +=((v,new IntVar(model,0,Int.MaxValue,0,"global violation of " + v.name)))
    registerConstrainedVariable(v)
  }

  /**performs the same operation as registerForViolation on the given variables*/
  def registerForViolation(vit:Iterable[Variable]){
    for (v <- vit) registerForViolation(_:Variable)
  }
  
  /**The degree of violation associated with the variable v.
   * The constraint system must have been closed prior to calling this method.
   * @param v must have been previously declared through the registerForViolation(v:Variable) method
   */
  override def getViolation(v:Variable):IntVar = Violations(v)

  /**Returns the global violation of the constraint system, that is the weighted sum of the violation of the posted constraints
   *close() should have been called prior to calling this method.
   */
  override def getViolation:IntVar = Violation
}
