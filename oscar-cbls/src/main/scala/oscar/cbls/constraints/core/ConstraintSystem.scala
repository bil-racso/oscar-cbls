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

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.numeric.{Prod, Prod2, Sum}
import oscar.cbls.objective.Objective

import scala.collection.immutable.SortedSet

/** A constraint system is a composition of constraints.
 * It is itself a constraint, offering the same features, namely, a global violation and a violation specific to each variable.
 * monitoring the violation of a variable requires that the ConstraintSystem has been notified that the variable should have an associated violation degree.
 * This is achieved by calling the method registerForViolation(v:Variable).
 * @param model is the model in which all the variables referenced by the constraints are declared.
 * @author renaud.delandtsheer@cetic.be
 */
case class ConstraintSystem(model:Store) extends Constraint with Objective{
  //ConstraintSystems do not act as invariant because everything is subcontracted

  model.addToCallBeforeClose(() => this.close())

  class GlobalViolationDescriptor(val Violation:CBLSIntVar){
    var AggregatedViolation:List[IntValue] = List.empty
  }

  val IndexForLocalViolationINSU = model.newStorageKey()
  val IndexForGlobalViolationINSU = model.newStorageKey()

  var Violation:ChangingIntValue = null

  private var PostedConstraints:List[(Constraint,IntValue)] = List.empty
  //private var AllVars:SortedMap[Variable,List[(Constraint,IntVar)]]=SortedMap.empty

  private var VarInConstraints:List[AbstractVariable] = List.empty
  private var VarsWatchedForViolation:List[AbstractVariable] = List.empty

  override def toString = {
    val constraints = PostedConstraints.map(_._1)
    val sortedConstraints = constraints.sortBy(c => c.violation.value)
    val sortedConstraintsStrings = sortedConstraints.map(c => "" + c.violation + " " + c)
    "ConstraintSystem{" + this.Violation + "\n " + sortedConstraintsStrings.mkString("\n  ") + "}\n"
  }

  /**
   * @return the constraints posted in the constraint system, together with their weighting factor.
   */
  def getPostedConstraints:List[(Constraint,IntValue)] = PostedConstraints

  /**Method used to post a constraint in the constraint system. (synonym of post)
    * Cannot be called after the constraint system has been closed.
    * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
    * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
    * @param c is the posted constraint.
    * @param weight is the weight that is used in the weighted sum of the violation degrees.
    */
  def add(c:Constraint,weight:IntValue=null) = post(c,weight)

  /**Method used to post a constraint in the constraint system. (synonym of add)
   * Cannot be called after the constraint system has been closed.
   * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
   * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
   * @param c is the posted constraint.
   * @param weight is the weight that is used in the weighted sum of the violation degrees.
   */
  def post(c:Constraint,weight:IntValue=null){

    PostedConstraints = (c,weight) :: PostedConstraints

    for(variable:AbstractVariable <- c.constrainedVariables){
      val oldConstrAndWeightList:List[(Constraint,IntValue)] = variable.getStorageAt(IndexForLocalViolationINSU,List.empty)
      if (oldConstrAndWeightList.isEmpty) VarInConstraints = variable :: VarInConstraints
      variable.storeAt(IndexForLocalViolationINSU,(c,weight)::oldConstrAndWeightList)
    }
  }

  private def aggregateLocalViolations(){
    for (variable <- VarInConstraints){
      val ConstrAndWeightList:List[(Constraint,IntValue)] = variable.getStorageAt(IndexForLocalViolationINSU,null)

      val product:List[IntValue] = ConstrAndWeightList.map((ConstrAndWeight) => {
        val constr = ConstrAndWeight._1
        val weight = ConstrAndWeight._2
        if(weight == null) constr.violation(variable)
        else Prod2(constr.violation(variable),weight)
      })
      val LocalViolation = if (!product.isEmpty && product.tail.isEmpty) product.head
                            else Sum(product)
      variable.storeAt(IndexForLocalViolationINSU,LocalViolation)
    }
  }

  private def PropagateLocalToGlobalViolations(){
    for(varWithLocalViol <- VarInConstraints){
      val localViol:IntValue = varWithLocalViol.getAndFreeStorageAt(IndexForLocalViolationINSU)
      val sources = model.getSourceVariables(varWithLocalViol)
      //TODO: this seems a bit inefficient
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
  def close(){
    if(!isClosed){
      isClosed = true
      Violation = new Sum(PostedConstraints.map((constraintANDintvar) => {
        if(constraintANDintvar._2 == null) constraintANDintvar._1.violation
        else Prod(List(constraintANDintvar._1.violation,constraintANDintvar._2))
      })).setName("violation")

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
  override def violation(v:AbstractVariable):CBLSIntVar = {
    val CPStoredRecord:GlobalViolationDescriptor = v.getStorageAt(IndexForGlobalViolationINSU,null)
    if (CPStoredRecord == null){
      if (model.isClosed) throw new Exception("cannot create new violation after model is closed.")
      //not registered yet
      VarsWatchedForViolation = v :: VarsWatchedForViolation
      val violationVariable = CBLSIntVar(model,0, 0 to Int.MaxValue,"global violation of " + v.name)
      v.storeAt(IndexForGlobalViolationINSU,new GlobalViolationDescriptor(violationVariable))
      registerConstrainedVariable(v)
      violationVariable
    }else{
      //already registered
      CPStoredRecord.Violation
    }
  }

  def violations[V<:Variable](vs:Array[V]):Array[CBLSIntVar] = {
    vs.map(violation(_))
  }

  /**Returns the global violation of the constraint system, that is the weighted sum of the violation of the posted constraints
   *close() should have been called prior to calling this method.
   */
  override def violation:ChangingIntValue = Violation

  /** to get the violated constraints, for debugging purpose
    * @return the constraints that are violated, and whose ponderation factor is not zero
    */
  def violatedConstraints:List[Constraint] =
    PostedConstraints.filter(p => (p._2 == null || p._2.value !=0) && !p._1.isTrue).map(p => p._1)

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {}

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override protected def value: Int = Violation.value
}

