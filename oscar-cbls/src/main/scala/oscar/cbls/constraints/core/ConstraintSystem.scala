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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.constraints.core

import oscar.cbls.invariants.core.computation.{Variable, CBLSIntVar, Store}
import oscar.cbls.objective.ObjectiveTrait
import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.lib.numeric.{Prod2, Prod, Sum}

/** A constraint system is a composition of constraints.
 * It is itself a constraint, offering the same features, namely, a global violation and a violation specific to each variable.
 * monitoring the violation of a variable requires that the ConstraintSystem has been notified that the variable should have an associated violation degree.
 * This is achieved by calling the method registerForViolation(v:Variable).
 * @author  Renaud De Landtsheer rdl@cetic.be
 * @param _model is the model in which all the variables referenced by the constraints are declared.
 */
case class ConstraintSystem(val _model:Store) extends Constraint with ObjectiveTrait{
  //ConstraintSystems do not act as invariant because everything is subcontracted.

  model = _model

  finishInitialization(_model)

  model.addToCallBeforeClose(_ => this.close())

  class GlobalViolationDescriptor(val Violation:CBLSIntVar){
    var AggregatedViolation:List[CBLSIntVar] = List.empty
  }

  val IndexForLocalViolationINSU = model.getStorageIndex
  val IndexForGlobalViolationINSU = model.getStorageIndex

  private val Violation:CBLSIntVar = CBLSIntVar(this.model,0,Int.MaxValue,0,"Violation")

  private var PostedConstraints:List[(Constraint,CBLSIntVar)] = List.empty
  //private var AllVars:SortedMap[Variable,List[(Constraint,IntVar)]]=SortedMap.empty

  private var VarInConstraints:List[Variable] = List.empty
  private var VarsWatchedForViolation:List[Variable] = List.empty

  override def toString() = {
    val constraints = PostedConstraints.map(_._1)
    val sortedConstraints = constraints.sortBy(c => c.violation.value)
    val sortedConstraintsStrings = sortedConstraints.map(c => "" + c.violation + " " + c)
    "ConstraintSystem{" + this.Violation + "\n " + sortedConstraintsStrings.mkString("\n  ") + "}\n"
  }
  /**
   * @return the constraints posted in the constraint system, together with their weighting factor.
   */
  def getPostedConstraints:List[(Constraint,CBLSIntVar)] = PostedConstraints

  /**Method used to post a constraint in the constraint system. (synonym of post)
    * Cannot be called after the constraint system has been closed.
    * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
    * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
    * @param c is the posted constraint.
    * @param weight is the weight that is used in the weighted sum of the violation degrees.
    */
  def add(c:Constraint,weight:CBLSIntVar=null) = post(c,weight)

  /**Method used to post a constraint in the constraint system. (synonym of add)
   * Cannot be called after the constraint system has been closed.
   * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
   * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
   * @param c is the posted constraint.
   * @param weight is the weight that is used in the weighted sum of the violation degrees.
   */
  def post(c:Constraint,weight:CBLSIntVar=null){

    assert(c.getPropagationStructure == this.model || c.getPropagationStructure == null,
      "constraints must be registered to same propagation structure as constraint system")
    PostedConstraints = (c,weight) :: PostedConstraints

    for(variable <- c.constrainedVariables){
      val oldConstrAndWeightList:List[(Constraint,CBLSIntVar)] = variable.getStorageAt(IndexForLocalViolationINSU,List.empty)
      if (oldConstrAndWeightList.isEmpty) VarInConstraints = variable :: VarInConstraints
      variable.storeAt(IndexForLocalViolationINSU,((c,weight)::oldConstrAndWeightList))
    }
  }

  private def aggregateLocalViolations(){
    for (variable <- VarInConstraints){
      val ConstrAndWeightList:List[(Constraint,CBLSIntVar)] = variable.getStorageAt(IndexForLocalViolationINSU,null)

      val product:List[CBLSIntVar] = ConstrAndWeightList.map((ConstrAndWeight) => {
        val constr = ConstrAndWeight._1
        val weight = ConstrAndWeight._2
        if(weight == null) constr.violation(variable)
        else (Prod2(constr.violation(variable),weight)).toIntVar
      })
      val LocalViolation = (if (!product.isEmpty && product.tail.isEmpty) product.head
                            else Sum(product).toIntVar)
      variable.storeAt(IndexForLocalViolationINSU,LocalViolation)
    }
  }

  private def PropagateLocalToGlobalViolations(){
    for(varWithLocalViol <- VarInConstraints){
      val localViol:CBLSIntVar = varWithLocalViol.getStorageAt(IndexForLocalViolationINSU)
      val sources = model.getSourceVariables(varWithLocalViol)
      for(sourcevar <- sources){
        val GlobalViol:GlobalViolationDescriptor = sourcevar.getStorageAt(IndexForGlobalViolationINSU,null)
        if (GlobalViol!=null) GlobalViol.AggregatedViolation = localViol :: GlobalViol.AggregatedViolation
      }
    }
  }

  private def aggregateGlobalViolations(){
    for (variable <- VarsWatchedForViolation){
      val ElementsAndViol:GlobalViolationDescriptor = variable.getStorageAt(IndexForGlobalViolationINSU)
      ElementsAndViol.Violation <== Sum(ElementsAndViol.AggregatedViolation)
      ElementsAndViol.AggregatedViolation = null
    }
  }

  var isClosed = false
  /**Must be invoked before the violation can be queried.
   * no constraint can be added after his method has been called.
   * this method must also be called before closing the model.
   */
  @deprecated("you do not need to call close on a ConstraintSystem, it is closed by the Model when the model is closed.","1.0")
  def close(){
    if(!isClosed){
      isClosed = true
      Violation <== Sum(PostedConstraints.map((constraintANDintvar) => {
        if(constraintANDintvar._2 == null) constraintANDintvar._1.violation
        else Prod(SortedSet(constraintANDintvar._1.violation,constraintANDintvar._2)).toIntVar
      }))

      setObjectiveVar(Violation)

      aggregateLocalViolations()
      PropagateLocalToGlobalViolations()
      aggregateGlobalViolations()
    }
  }

  /**Call this method to notify that the variable should have a violation degree computed for the whole constraint system.
   * it is not compulsory that the variable is directly involved in the constraints. It can be involved indirectly, even through invariants.
   * The variables registered here are the ones and only ones that are considered as constrained by the constraint system, and returned by the method constrainedVariables
   * The violation mechanism will be instantiated as many times as the variable is registered for violation. 
   * @param v the variable that is registered
   */
  @deprecated("you can directly ask for a violation, just that you need to to it before constraint system is closed","1.0")
  def registerForViolation(v:Variable){
    violation(v:Variable)
  }

  /**performs the same operation as registerForViolation on the given variables*/
  @deprecated("you can directly ask for a violation, just that you need to to it before constraint system is closed","1.0")
  def registerForViolation(vit:Iterable[Variable]){
    for (v <- vit) registerForViolation(_:Variable)
  }
  
  /**The degree of violation associated with the variable v.
   * The constraint system must have been closed prior to calling this method.
   * @param v must have been previously declared through the registerForViolation(v:Variable) method
   */
  override def violation(v:Variable):CBLSIntVar = {
    val CPStoredRecord:GlobalViolationDescriptor = v.getStorageAt(IndexForGlobalViolationINSU,null)
    if (CPStoredRecord == null){
      if (model.isClosed) throw new Exception("cannot create new violation after model is closed.")
      //not registered yet
      VarsWatchedForViolation = v :: VarsWatchedForViolation
      val violationvariable = CBLSIntVar(model,0,Int.MaxValue,0,"global violation of " + v.name)
      v.storeAt(IndexForGlobalViolationINSU,new GlobalViolationDescriptor(violationvariable))
      registerConstrainedVariable(v)
      violationvariable
    }else{
      //already registered
      CPStoredRecord.Violation
    }
  }

  /**Returns the global violation of the constraint system, that is the weighted sum of the violation of the posted constraints
   *close() should have been called prior to calling this method.
   */
  override def violation:CBLSIntVar = Violation

  /** to get the violated constraints, for debugging purpose
    * @return the constraints that are violated, and whose ponderation factor is not zero
    */
  def violatedConstraints:List[Constraint] =
    PostedConstraints.filter(p => (p._2 != null || p._2.value !=0) && p._1.isTrue).map(p => p._1)
}
