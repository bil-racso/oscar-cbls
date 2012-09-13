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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.invariants.core.computation

import collection.immutable.{SortedSet, SortedMap};
import oscar.cbls.invariants.core.algo.dag._;
import oscar.cbls.invariants.core.propagation._;

/**This class contains the model, namely, the invariants and variables
 * They are all modeled as propagation Elements, which are handled by the inherited propagationstructure class.
 *
 * @param Verbose requires that the propagation structure prints a trace of what it is doing. all prints are preceded by ''PropagationStruture''
 * @param DebugMode specifies that once propagation is finished, it must call the checkInternals method on all propagation elements.
 * @param NoCycle is to be set to true only if the static dependency graph between propagation elements has no cycles. If unsure, set to false, the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
 */
class Model(override val Verbose:Boolean = false,
            override val DebugMode:Boolean = false,
            override val NoCycle:Boolean = false,
            override val TopologicalSort:Boolean = false)
  extends PropagationStructure(Verbose,DebugMode,NoCycle,TopologicalSort){

  private var Variables:List[Variable] = List.empty
  private var Invariants:List[Invariant] = List.empty
  private var PropagationElements:List[PropagationElement] = List.empty
  private var Closed:Boolean=false

  def isClosed = Closed

  /**To save the current value of the variables registered in the model
   * @param OnlyPrimitive if set to true (as by default) the solution will only contain the variables that are not derived through an invariant
   */
  def getSolution(OnlyPrimitive:Boolean = true):Solution = {
    var assignationInt:SortedMap[IntVar,Int] = SortedMap.empty
    var assignationIntSet:SortedMap[IntSetVar,SortedSet[Int]] = SortedMap.empty
    for (v:Variable <- Variables if !OnlyPrimitive || v.getDefiningInvariant == null){
      if(v.isInstanceOf[IntVar]){
        assignationInt += ((v.asInstanceOf[IntVar], v.asInstanceOf[IntVar].getValue()))
      }else if(v.isInstanceOf[IntSetVar]){
        assignationIntSet += ((v.asInstanceOf[IntSetVar], v.asInstanceOf[IntSetVar].getValue()))
      }
    }
    Solution(assignationInt,assignationIntSet,this)
  }

  /**this is to be used as a backtracking point in a search engine
   * you can only save variables that are not controlled*/
  def saveValues(vars:Variable*):Snapshot = {
    var assignationInt:List[(IntVar,Int)] = List.empty
    var assignationIntSet:List[(IntSetVar,SortedSet[Int])] = List.empty
    for (v:Variable <- vars if v.getDefiningInvariant == null){
      if(v.isInstanceOf[IntVar]){
        assignationInt = ((v.asInstanceOf[IntVar], v.asInstanceOf[IntVar].getValue(true))) :: assignationInt
      }else if(v.isInstanceOf[IntSetVar]){
        assignationIntSet = ((v.asInstanceOf[IntSetVar], v.asInstanceOf[IntSetVar].getValue(true))) :: assignationIntSet
      }
    }
    Snapshot(assignationInt,assignationIntSet,this)
  }

  /**To restore a saved solution
   * notice that only the variables that are not derived will be restored; others will be derived lazily at the next propagation wave.
   * This enables invariants to rebuild their internal data structure if needed.
   * Only solutions saved from the same model can be restored in the model.
   */
  def restoreSolution(s:Solution){
    assert(s.model==this)
    for((intsetvar,intset) <- s.assignationIntSet if intsetvar.getDefiningInvariant == null){
      intsetvar := intset
    }
    for((intvar,int) <- s.assignationInt if intvar.getDefiningInvariant == null){
      intvar := int
    }
  }

  /**To restore a saved snapshot
   * notice that only the variables that are not derived will be restored; others will be derived lazily at the next propagation wave.
   * This enables invariants to rebuild their internal data structure if needed.
   * Only solutions saved from the same model can be restored in the model.
   */
  def restoreSnapshot(s:Snapshot){
    assert(s.model==this)
    for((intsetvar,intset) <- s.assignationIntSet if intsetvar.getDefiningInvariant == null){
      intsetvar := intset
    }
    for((intvar,int) <- s.assignationInt if intvar.getDefiningInvariant == null){
      intvar := int
    }
  }

  /**Called by each variable to register themselves to the model
   * @param v the variable
   * @return a unique identifier that will be used to distinguish variables. basically, variables use this value to set up an arbitrary ordering for use in dictionnaries.
   */
  def registerVariable(v:Variable):Int = {
    assert(!Closed,"model is closed, cannot add variables")
    //ici on utilise des listes parce-que on ne peut pas utiliser des dictionnaires
    // vu que les variables n'ont pas encore recu leur unique ID.
    Variables = v :: Variables
    PropagationElements =  v :: PropagationElements
    GetNextID()
  }

  /**Called by each invariants to register themselves to the model
   * @param i the invariant
   * @return a unique identifier that will be used to distinguish invariants. basically, invariants use this value to set up an arbitrary ordering for use in dictionnaries.
   */
  def registerInvariant(i:Invariant):Int = {
    assert(!Closed,"model is closed, cannot add invariant")
    Invariants = i :: Invariants
    PropagationElements = i :: PropagationElements
    GetNextID()
  }

  override def getPropagationElements:Iterable[PropagationElement] = {
    PropagationElements
  }

  /**calls this when you have declared all your invariants and variables.
   * This must be called before any query and update can be made on the model, and after all the invariants and variables have been declared.
   */
  def close(DropStaticGraph: Boolean = true){
    assert(!Closed, "cannot close a model twice")
    setupPropagationStructure(DropStaticGraph)
    Closed=true
  }

  /**this checks that invariant i is one that is supposed to do something now
   * used to check that invariants have declared all their controling links to the model
   * */
  def checkExecutingInvariantOK(i:Invariant):Boolean = {
    if(i != null){
      if (NotifiedInvariant != null && NotifiedInvariant != i){
        return false
      }
      if (NotifiedInvariant == null && getPropagatingElement != null &&  getPropagatingElement != i){
        return false
      }
    }else{
      if (NotifiedInvariant != null || getPropagatingElement != null){
        return false
      }
    }
    true
  }

  var NotifiedInvariant:Invariant=null

  override def toString:String = Variables.toString()
  def toStringInputOnly = Variables.filter(v => v.getDefiningInvariant == null).toString()

  //returns the set of source variable that define this one.
  // This exploration procedure explores passed dynamic invariants,
  // but it over-estimates the set of source variables over dynamic invariants, as it uses the static graph.
  def getSourceVariables(v:Variable):SortedSet[Variable] = {
    var ToExplore: List[PropagationElement] = List(v)
    var SourceVariables:SortedSet[Variable] = SortedSet.empty[Variable]
    //TODO: check that this works also in case of cycle.
    while(!ToExplore.isEmpty){
      val head = ToExplore.head
      ToExplore = ToExplore.tail
      if(head.isInstanceOf[Variable]){
        val v:Variable = head.asInstanceOf[Variable]
        if(!SourceVariables.contains(v)){
          SourceVariables += v
          val definv = v.getDefiningInvariant
          if (definv != null){
            ToExplore = definv :: ToExplore
          }
        }
      }else if(head.isInstanceOf[Invariant]){
        val i:Invariant = head.asInstanceOf[Invariant]
        for (listened <- i.getStaticallyListenedElements){
          if (listened.getPropagationStructure != null && (!listened.isInstanceOf[Variable] || !SourceVariables.contains(listened.asInstanceOf[Variable]))){
            ToExplore = listened :: ToExplore
          }
        }
      }else{assert(false,"propagation element that is not a variable, and not an invariant??")}
    }
    SourceVariables
  }
}

case class Snapshot(assignationInt:List[(IntVar, Int)],
                    assignationIntSet:List[(IntSetVar,SortedSet[Int])],
                    model:Model)

/**This class contains a solution. It can be generated by a model, to store the state of the search, and restored.
 * it remains linked to the model, as it maintains references to the variables declared in the model.
 * you cannot pass it over a network connexion for instance.
 * see methods getSolution and restoreSolution in [[oscar.cbls.invariants.core.computation.Model]]
 */
case class Solution(assignationInt:SortedMap[IntVar,Int],
                    assignationIntSet:SortedMap[IntSetVar,SortedSet[Int]],
                    model:Model){
  
  /**to get the value of an IntVar in the saved solution*/
  def getSnapshot(v:IntVar):Int = assignationInt(v)

  /**to get the value of an IntSetVar in the saved solution*/
  def getSnapshot(v:IntSetVar):SortedSet[Int] = assignationIntSet(v)

  /**converts the solution to a human-readable string*/
  override def toString:String = {
    var acc:String = ""
    var started = false
    acc = "IntVars("
    for(v <-assignationInt){
      if(started) acc += ","
      started = true
      acc += v._1.name + ":=" + v._2
    }
    acc +=")  IntSetVars("
    started = false
    for(v <-assignationIntSet){
      if(started) acc += ","
      started = true
      acc += v._1.name + ":=" + (if (v._2.isEmpty){"null"}else{"{" + v._2.foldLeft("")((acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"})
    }
    acc + ")"
  }
}

object Invariant{
  implicit val Ord:Ordering[Invariant] = new Ordering[Invariant]{
    def compare(o1: Invariant, o2: Invariant) = o1.compare(o2)
  }
}

/**This is be base class for all invariants.
 * Invariants also register to the model, but they identify the model they are associated to by querying the variables they are monitoring.
 *
 */
trait Invariant extends PropagationElement{
  var model:Model = null

  def getPropagationStructure = this.model

  /**Must be called by all invariant after they complete their initialization
   * that is: before they get their output variable.
   * This performs some registration to the model, which is discovered by exploring the variables that are statically registered to the model
   * no more variable can be registered statically after this method has been called.
   * @param model; if specified, it only checks that the model is coherent, and registers to it for the ordering
   */
  def finishInitialization(model:Model = null){
    if (model == null){
      this.model = InvariantHelper.FindModel(getStaticallyListenedElements)
    }else{
      this.model = model //assert(model == InvariantHelper.FindModel(getStaticallyListenedElements()))
    }
    if (this.model!= null){
      UniqueID = this.model.registerInvariant(this)
    }else{
      UniqueID = -1
    }
  }

  //TODO: these methods should be in PropagationElement, not in Invariants!!!

  /**Call this from within the invariant to notify that you will statically listen to this variable.
   * You CANNOT register a variable twice. It is undetected, but will lead to unexpected behavior.
   * @param v the variable that you want to listen to (and be notified about change)
   */
  def registerStaticDependency(v:Variable){
    registerStaticallyListenedElement(v)
  }

  def registerStaticDependencies(v:Variable*){
    for (vv <- v)registerStaticDependency(vv)
  }

  def registerStaticDependencyAll(v:Iterable[Variable]){
    for (vv <- v)registerStaticDependency(vv)
  }

  /**register to determining element. It must be in the static dependency graph*/
  def registerDeterminingDependency(v:Variable,i:Any = -1){
    registerDeterminingElement(v,i)
  }

  /**this method is an alternative to the two call to the registration methods
   * it performs the registration to the static and dynamic graphs at once.
   * just to spare one call
   * @param v the variable that we want to register to
   * @param i the integer value that will be passed to the invariant to notify some changes in the value of this variable
   */
  def registerStaticAndDynamicDependency(v:Variable,i:Any = -1){
    registerStaticDependency(v)
    registerDynamicDependency(v,i)
  }

  def registerStaticAndDynamicDependencies(v:((Variable,Any))*){
    for (varint <- v){
      registerStaticDependency(varint._1)
      registerDynamicDependency(varint._1,varint._2)
    }
  }

  def registerStaticAndDynamicDependencyAll(v:Iterable[((Variable,Any))]){
    for (varint <- v){
      registerStaticDependency(varint._1)
      registerDynamicDependency(varint._1,varint._2)
    }
  }

  /**
   * registers static and dynamic dependency to all items in the array
   * let be i, a position in the array, the idenx used for dynamic dependency registration is i+offset
   * @param v the variable that are registered
   * @param offset and offset applied to the position in the array when reigstering the dynamic dependency
   */
  def registerStaticAndDynamicDependencyArrayIndex[T <: Variable](v:Array[T],offset:Int = 0){
    for (i <- v.indices){
      registerStaticDependency(v(i))
      registerDynamicDependency(v(i),i + offset)
    }
  }

  def registerStaticAndDynamicDependenciesNoID(v:Variable*){
    for (varint <- v){
      registerStaticDependency(varint)
      registerDynamicDependency(varint)
    }
  }

  def registerStaticAndDynamicDependencyAllNoID(v:Iterable[Variable]){
    for (varint <- v){
      registerStaticDependency(varint)
      registerDynamicDependency(varint)
    }
  }

  /**Call this from within the invariant to notify that you will listen to this variable.
   * The variable must be registered in the static propagation graph.
   * You CANNOT register a variable twice. It is undetected, but will lead to unexpected behavior.
   * @param v the variable that you want to listen to (and be notified about change)
   * @param i: an integer value that will be passed when updates on this variable are notified to the invariant
   * @return a handle that is required to remove the listened var from the dynamically listened ones
   */
  def registerDynamicDependency(v:Variable,i:Any = -1):KeyForElementRemoval = {
    registerDynamicallyListenedElement(v,i)
  }

  /**Call this from within the invariant to notify that you stop listen to this variable.
   * The variable must have been be registered in the static propagation graph.
   * @param k the handle that was returned when the variable added to the dynamic graph
   */
  def unregisterDynamicDependency(k:KeyForElementRemoval){
    unregisterDynamicallyListenedElement(k)
  }

  //we are only notified for the variable we really want to listen (cfr. mGetReallyListenedElements, registerDynamicDependency, unregisterDynamicDependency)
  def notifyIntChangedAny(v:IntVar,i:Any,OldVal:Int,NewVal:Int){notifyIntChanged(v,i.asInstanceOf[Int], OldVal,NewVal)}

  def notifyIntChanged(v:IntVar,i:Int,OldVal:Int,NewVal:Int){notifyIntChanged(v,OldVal,NewVal)}

  def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){}

  def notifyInsertOnAny(v:IntSetVar,i:Any,value:Int){notifyInsertOn(v,i.asInstanceOf[Int],value)}

  def notifyInsertOn(v:IntSetVar,i:Int,value:Int){notifyInsertOn(v,value)}

  def notifyInsertOn(v:IntSetVar,value:Int){}

  def notifyDeleteOnAny(v:IntSetVar,i:Any,value:Int){notifyDeleteOn(v,i.asInstanceOf[Int],value)}

  def notifyDeleteOn(v:IntSetVar,i:Int,value:Int){notifyDeleteOn(v,value)}

  def notifyDeleteOn(v:IntSetVar,value:Int){}

  /**To override whenever possible to spot errors in invariants.
   * this will be called for each invariant after propagation is performed.
   * It requires that the Model is instantiated with the varible debug set to true.
   */
  override def checkInternals(){;}

  def getDotNode = "[label = \"" + this.getClass.getSimpleName + "\" shape = box]"
}

object InvariantHelper{
  /**this is useful to find the model out of a set of propagation elements.
   *
   * @param i some propagation elements, typically, variables lsitened by some invariants
   * @return the model that the invariant belongs to
   */
  def FindModel(i:Iterable[PropagationElement]):Model={
    var toreturn:Model = null
    i.foreach(e => {
      if (e.isInstanceOf[Variable]){
        val m = e.asInstanceOf[Variable].model
        if (m != null){
          if (toreturn != null){assert(m == toreturn)}
          toreturn = m
          if (!m.DebugMode) return m
        }
      }
    })
    toreturn //they are all constants, so what are you doing here??
  }
}

/**This is the base class for variable. A variable is a propagation element that holds some value.
 * Variables have an associated model, to which they register as soon as they are created. Variables also have a name,
 * which is used solely for printing models.
 */
abstract class Variable(val model:Model,val name:String) extends PropagationElement{
  UniqueID = if (model == null) -1 else model.registerVariable(this)

  def getPropagationStructure = this.model

  var DefiningInvariant:Invariant = null

  def setDefiningInvariant(i:Invariant) {
    assert(i.model == model || i.model == null)
    if(DefiningInvariant == null){
      DefiningInvariant = i
      registerStaticallyListenedElement(i)
      registerDynamicallyListenedElement(i,0)
    }else{
      throw new Exception("variable [" + name + "] cannot have more than one defining invariant")
    }
  }
  def getDefiningInvariant:Invariant = DefiningInvariant

  /**this method s to be called by any method that internally modifies the value of the variable
   * it schedules the variable for propagation, and performs a basic check of the identify of the executing invariant*/
  def notifyChanged(){
    //modifier le test.
    if (this.model == null ||(!this.model.isClosed && this.getDynamicallyListeningElements.isEmpty)){
      performPropagation()
    }else{
      assert(model.checkExecutingInvariantOK(DefiningInvariant),"variable [" + this + "] affected by non-controlling invariant")
      scheduleForPropagation()
    }
  }

  def getDotColor:String = {
    if (getStaticallyListeningElements.isEmpty){
      "blue"
    }else if (getStaticallyListenedElements.isEmpty){
      "green"
    }else{
      "black"
    }
  }
}

object Variable{
  implicit val ord:Ordering[Variable] = new Ordering[Variable]{
    def compare(o1: Variable, o2: Variable) = o1.compare(o2)
  }
}

object Event{
  //TODO: on peut pas ettre des params par défaut si on fait plusieurs apply.

  def apply(v:Variable,
            action: =>Unit):Event = {
    val toreturn = new Event(v,null,null)
    toreturn.setAction((_:Unit) => {action})
//    if (intaction != null) toreturn.setIntAction(intaction)
 //   if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    toreturn
  }

  def apply(v:Variable,
            action: =>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,null,ModifiedVars)
    toreturn.setAction((_:Unit) => {action})
    //    if (intaction != null) toreturn.setIntAction(intaction)
    //   if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    toreturn
  }

  /**this is an event, which is used to run dedicated code when the value of some variable changes.
   * It can also impact on the value of other variable, although it is not recommended
   * to implement invariants as events, because you cannot have the delta.
   * @param v the variable whose change will trigger the execution of action
   */
  def apply(v:Variable, w:Variable,
            intaction:Int=>Unit):Event = {
    val toreturn = new Event(v,w,null)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  /**this is an event, which is used to run dedicated code when the value of some variable changes.
   * It can also impact on the value of other variable, although it is not recommended
   * to implement invariants as events, because you cannot have the delta.
   * @param v the variable whose change will trigger the execution of action
   * @param ModifiedVars the variables that could be modified by the Event
   */
  def apply(v:Variable, w:Variable,
            intaction:Int=>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,w,ModifiedVars)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  def apply(v:Variable,
            intaction:Int=>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,null,ModifiedVars)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  def apply(v:Variable,
            intaction:Int=>Unit):Event = {
    val toreturn = new Event(v,null,null)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

/*  def apply(v:Variable, w:Variable,
            action: =>Unit = null,
            intaction:Int=>Unit = null, 
            intsetaction:SortedSet[Int]=>Unit = null,
            intintaction: (Int,Int)=>Unit = null,
            intsetintsetaction:(SortedSet[Int],SortedSet[Int]) => Unit = null,
            intsetintaction:(SortedSet[Int],Int) => Unit = null,
            intintsetaction:(Int,SortedSet[Int]) => Unit = null,
            ModifiedVars:Iterable[Variable] = null):Event = {
    val toreturn = new Event(v,w,ModifiedVars)
    toreturn.setAction((_:Unit) => {action})
    if (intaction != null) toreturn.setIntAction(intaction)
    if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    if (intintaction!=null) toreturn.setintintaction(intintaction)
    if (intsetintsetaction!=null) toreturn.setintsetintsetaction(intsetintsetaction)
    if (intsetintaction!=null) toreturn.setintsetintaction(intsetintaction)
    if (intintsetaction!=null) toreturn.setintintsetaction(intintsetaction)
    toreturn
  }
  */
}

/**Use the apply method in the companion object for building this*/
class Event(v:Variable, w:Variable, ModifiedVars:Iterable[Variable]) extends Invariant{
  //unfortunately, it is not possible to pass a type "=>Unit" as parameter to a case class.

  private var action: (Unit=>Unit)=null
  private var actionIntParam: (Int=>Unit) = null
  private var actionIntSetParam: (SortedSet[Int] => Unit) = null

  private var oldIntv = 0;
  private var oldIntSetv:SortedSet[Int] = SortedSet.empty
  private var oldIntw = 0;
  private var oldIntSetw:SortedSet[Int] = SortedSet.empty

  private var intintaction: ((Int,Int) => Unit) = null
  private var intsetintsetaction:((SortedSet[Int],SortedSet[Int]) => Unit) = null
  private var intsetintaction:((SortedSet[Int],Int) => Unit) = null
  private var intintsetaction:((Int,SortedSet[Int]) => Unit) = null

  def setAction(action: Unit=>Unit){
    this.action = action
  }
  def setIntAction(action: Int=>Unit){
    this.actionIntParam = action
    oldIntv = v.asInstanceOf[IntVar].getValue()
  }
  def setIntSetAction(action: SortedSet[Int] => Unit){
    this.actionIntSetParam = action
    oldIntSetv = v.asInstanceOf[IntSetVar].value
  }

  def setintintaction(intintaction: (Int,Int)=>Unit){
    this.intintaction = intintaction
    this.oldIntv = v.asInstanceOf[IntVar].getValue()
    this.oldIntw = w.asInstanceOf[IntVar].getValue()
  }

  def setintsetintsetaction(intsetintsetaction:(SortedSet[Int],SortedSet[Int]) => Unit){
    this.intsetintsetaction = intsetintsetaction
    this.oldIntSetv = v.asInstanceOf[IntSetVar].value
    this.oldIntSetw = w.asInstanceOf[IntSetVar].value
  }

  def setintsetintaction(intsetintaction:(SortedSet[Int],Int) => Unit){
    this.intsetintaction = intsetintaction
    this.oldIntSetv = v.asInstanceOf[IntSetVar].value
    this.oldIntw = w.asInstanceOf[IntVar].getValue()
  }

  def setintintsetaction(intintsetaction:(Int,SortedSet[Int]) => Unit){
    this.intintsetaction = intintsetaction
    this.oldIntv = v.asInstanceOf[IntVar].getValue()
    this.oldIntSetw = w.asInstanceOf[IntSetVar].value
  }

  registerStaticAndDynamicDependency(v)
  if(w!=null)  registerStaticAndDynamicDependency(w)
  finishInitialization()
  if (ModifiedVars != null)
    for(variable <- ModifiedVars){variable.setDefiningInvariant(this)}

  override def notifyIntChanged(v:IntVar,i:Int,OldVal:Int,NewVal:Int){scheduleForPropagation()}
  override def notifyInsertOn(v:IntSetVar,i:Int,value:Int){scheduleForPropagation()}
  override def notifyDeleteOn(v:IntSetVar,i:Int,value:Int){scheduleForPropagation()}

  override def performPropagation(){
    if (action != null) action()

    if (actionIntParam!= null){
      actionIntParam(oldIntv)
    }
    if (actionIntSetParam != null){
      actionIntSetParam(oldIntSetv)
    }
    if(intintaction!=null){
      intintaction(oldIntv,oldIntw)
    }
    if (intsetintsetaction!=null){
      intsetintsetaction(oldIntSetv,oldIntSetw)
    }
    if (intsetintaction!=null){
      intsetintaction(oldIntSetv,oldIntw)
    }
    if (intintsetaction != null){
      intintsetaction(oldIntv,oldIntSetw)
    }

    //updating internal vars

    if (actionIntParam!= null){
      oldIntv = v.asInstanceOf[IntVar].getValue()
    }
    if (actionIntSetParam != null){
      oldIntSetv = v.asInstanceOf[IntSetVar].value
    }
    if(intintaction!=null){
      oldIntv = v.asInstanceOf[IntVar].getValue()
      oldIntw = w.asInstanceOf[IntVar].getValue()
    }
    if (intsetintsetaction!=null){
      oldIntSetv = v.asInstanceOf[IntSetVar].value
      oldIntSetw = w.asInstanceOf[IntSetVar].value
    }
    if (intsetintaction!=null){
      oldIntSetv = v.asInstanceOf[IntSetVar].value
      oldIntw = w.asInstanceOf[IntVar].getValue()
    }
    if (intintsetaction != null){
      oldIntv = v.asInstanceOf[IntVar].getValue()
      oldIntSetw = w.asInstanceOf[IntSetVar].value
    }
  }
}

/**an intvar is a variable managed by the [[oscar.cbls.invariants.core.computation.Model]] whose type is integer.
 *
 * @param model is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.IntConst]]
 * @param MinVal is the minimum value of the variable. Some invariants exploit this value to declare fixed size arrays
 * @param MaxVal is the maximum value of the variable. Some invariants exploit this value to declare fixed size arrays
 * @param Value is the value of the variable
 * @param name is the name of the variable, used for pretty printing only
 */
class IntVar(model:Model,val MinVal:Int,val MaxVal:Int,var Value:Int,override val name:String="")
  extends Variable(model,name) {

  {assert(MinVal <= MaxVal)}
  private var OldValue:Int=Value

  def getDomain:Range = new Range(MinVal,MaxVal,1)

  override def toString:String = name + ":=" + Value //getValue()

  def setValue(v:Int){
    if (v != Value){
      Value = v
      notifyChanged()
    }
  }
  
  def value:Int = getValue(false)

  def getValue(NewValue:Boolean=false):Int = {
    if(NewValue){
      assert(model.checkExecutingInvariantOK(DefiningInvariant),"variable [" + this
        + "] queried for latest val by non-controlling invariant")
      Value
    } else{
      if (this.DefiningInvariant!= null && model != null){
        model.propagate(this)
        OldValue
      }else{
        Value
      }
    }
  }
  


  implicit def apply:Int = this.getValue()

  override def performPropagation(){
    if(OldValue!=Value){
      val old=OldValue
      OldValue=Value
      for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
        val inv:Invariant = e._1.asInstanceOf[Invariant]
        assert({this.model.NotifiedInvariant=inv; true})
        inv.notifyIntChangedAny(this,e._2,old,Value)
        assert({this.model.NotifiedInvariant=null; true})
      }
    }
  }

  def :=(v:Int) {setValue(v)}
  def :+=(v:Int) {setValue(v+getValue(true))}
  def :-=(v:Int) {setValue(getValue(true) - v)}

  def ++ {this := this.getValue(true) +1}

  /**this operators swaps the value of two IntVar*/
  def :=:(v:IntVar){
    val a:Int = v.getValue()
    v:=this.getValue()
    this := a
  }

  def <==(i:IntInvariant) {i.setOutputVar(this)}
  def <==(i:IntVar) {this <== i.getClone}

  def getClone:IdentityInt = IdentityInt(this)

  override def checkInternals(){
    assert( OldValue == Value,this)
  }

  def getDotNode = "[label = \"IntVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object IntVar{
  
  def apply(model: Model, minVal:Int, maxVal:Int, value:Int, name:String) = {
    new IntVar(model,minVal,maxVal,value,name)
  }
  
  def apply(model: Model, domain: Range, value:Int, name:String="") = {
    new IntVar(model,domain.start,if (domain.isInclusive) domain.end else domain.end-1,value,name)
  }
  
  

  implicit val ord:Ordering[IntVar] = new Ordering[IntVar]{
    def compare(o1: IntVar, o2: IntVar) = o1.compare(o2)
  }

  def intVarToIntSetVar(i:IntVar):Singleton = Singleton(i)

  implicit def int2IntVar(a:Int):IntVar = IntConst(a)
}

/**
 * An IntConst is an IntVar that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param ConstValue: the value of the constant
 */
case class IntConst(ConstValue:Int, override val model:Model = null)
  extends IntVar(model,ConstValue,ConstValue,ConstValue,toString){
  override def getValue(NewValue:Boolean=false):Int = ConstValue //pour pas avoir de propagation
  override def toString:String = "IntConst("+ ConstValue + ")"
}

/**An IntSetVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Model]] whose type is set of integer.
 * @param model is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.IntSetConst]]
 * @param MinVal is the minimum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays
 * @param MaxVal is the maximum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays.
 * @param Value is the value of the variable
 * @param name is the name of the variable, used for pretty printing only
 */

class IntSetVar(override val model:Model,
                private val MinVal:Int,
                private val MaxVal:Int,
                override val name:String,
                private var Value:SortedSet[Int]=SortedSet.empty)
  extends Variable(model,name){

  //bulk assign starts all
  //then, value -> add/delete
  //basically the bulk assign is the main issue here.
  //maybe invariants should specify to the var the kind of update they are interested in??

  def getMinVal:Int = MinVal
  def getMaxVal:Int = MaxVal
  def getModel = model

  private var ToPerform: List[(Int,Boolean)] = List.empty
  private var OldValue:SortedSet[Int] = Value

  private def Perform{
    def Update(l:List[(Int,Boolean)]){
      if (l.isEmpty) return
      Update(l.tail)
      val (v, inserted) = l.head
      if (inserted) OldValue += v
      else OldValue -= v
    }
    Update(ToPerform)
    ToPerform = List.empty
  }

  override def checkInternals(){
    assert(this.DefiningInvariant == null || OldValue.intersect(Value).size == Value.size,
      "internal error: " + "Value: " + Value + " OldValue: " + OldValue)
  }

  override def toString:String = name + ":={" + getValue(true).foldLeft("")(
    (acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"

  /**The values that have bee impacted since last propagation was performed.
   * null if set was assigned
   */
  var TouchedValues:List[(Int,Boolean)] = List.empty

  def insertValue(v:Int){
    if (!Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,true) :: TouchedValues
      Value +=v
      notifyChanged()
    }
  }

  def deleteValue(v:Int){
    if (Value.contains(v)){
      if (TouchedValues != null) TouchedValues = (v,false) :: TouchedValues
      Value -=v
      notifyChanged()
    }
  }

  /**We suppose that the new value is not the same as the actual value.
   * otherwise, there is a huge waste of time.
   * @param v the new value to set to the variable
   */
  def setValue(v:SortedSet[Int]){
    TouchedValues = null
    Value = v
    notifyChanged()
  }

  override def performPropagation(){
    if(getDynamicallyListeningElements.isEmpty){
      //no need to do it gradually
      OldValue=Value
    }else{
      if (TouchedValues == null){
        //need to calll every listening one, so gradual approach required
        OldValue.diff(Value).foreach(v => {
          OldValue -=v
          for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.getModel.NotifiedInvariant=inv; true})
            inv.notifyDeleteOnAny(this,e._2,v)
            assert({this.getModel.NotifiedInvariant=null; true})
          }
        })
        //puis on fait partir tous les insert
        Value.diff(OldValue).foreach(v => {
          OldValue += v
          for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
            val inv:Invariant = e._1.asInstanceOf[Invariant]
            assert({this.getModel.NotifiedInvariant=inv; true})
            inv.notifyInsertOnAny(this,e._2,v)
            assert({this.getModel.NotifiedInvariant=null; true})
          }
        })
        //puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
        assert(OldValue.intersect(Value).size == Value.size, "mismatch: OLD" + OldValue + " New:" + Value)
        OldValue=Value
      }else{
        //only touched values must be looked for
        for ((v,inserted) <- TouchedValues.reverse){
          //TODO: we simply replay the history, so if some backtrack was performed, it sucks ;
          // eg: if something was propagated to this during a neighbourhood exploration not involving this var
          if (inserted){
            //inserted
            //TODO: this lazy mechanics might be unnecessary if invar queries it anyway...
            ToPerform = (v, inserted) :: ToPerform
            for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
              val inv:Invariant = e._1.asInstanceOf[Invariant]
              assert({this.getModel.NotifiedInvariant=inv;true})
              inv.notifyInsertOnAny(this,e._2,v)
              assert({this.getModel.NotifiedInvariant=null;true})
            }
          }else{
            //deleted
            ToPerform = (v, inserted) :: ToPerform
            for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){
              val inv:Invariant = e._1.asInstanceOf[Invariant]
              assert({this.getModel.NotifiedInvariant=inv;true})
              inv.notifyDeleteOnAny(this,e._2,v)
              assert({this.getModel.NotifiedInvariant=null;true})
            }
          }
        }
        OldValue = Value //in case we were lazy on the update
        ToPerform = List.empty
      }
    }
    TouchedValues = List.empty
  }

  def value:SortedSet[Int] = getValue(false)

  def getValue(NewValue:Boolean=false):SortedSet[Int] = {
    if (NewValue){
      assert(getModel.checkExecutingInvariantOK(DefiningInvariant),
        "variable [" + this + "] queried for latest val by non-controlling invariant")
      Value
    }else{
      if(this.DefiningInvariant!= null && getModel != null){
        getModel.propagate(this)
        if (!ToPerform.isEmpty){Perform}
        OldValue
      }else{
        Value
      }
    }
  }

  /**Use this to specify that the IntSetVar is the output of the IntSetInvariant*/
  def <==(i:IntSetInvariant){i.setOutputVar(this)}

  /**We suppose that the new value is not the same as the actual value.
   * otherwise, there is a huge waste of time.
   * @param v the new value to set to the variable
   */
  def :=(v:SortedSet[Int]) {setValue(v)}

  def :+=(i:Int) {this.insertValue(i)}
  def :-=(i:Int) {this.deleteValue(i)}

  def getDotNode = "[label = \"IntSetVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object IntSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.getValue()

  implicit val ord:Ordering[IntSetVar] = new Ordering[IntSetVar]{
    def compare(o1: IntSetVar, o2: IntSetVar) = o1.compare(o2)
  }

  implicit def intSet2IntSetVar(a:SortedSet[Int]):IntSetVar = IntSetConst(a)
}

/**
 * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param ConstValue: the value of the constant
 */
case class IntSetConst(ConstValue:SortedSet[Int],override val model:Model = null)
  extends IntSetVar(model
    ,if(ConstValue.isEmpty) Int.MinValue else ConstValue.min
    ,if(ConstValue.isEmpty) Int.MaxValue else ConstValue.max
    ,toString,ConstValue){
  override def getValue(NewValue:Boolean=false):SortedSet[Int] = ConstValue //pour pas avoir de propagation
  override def toString:String = "IntSetConst{" + ConstValue.foldLeft("")(
    (acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"
}

object Implicits{
  implicit def ToIntVar(i:IntInvariant):IntVar = i.toIntVar
  implicit def ToIntSetVar(i:IntSetInvariant):IntSetVar = i.toIntSetVar
  implicit def Int2IntVar(a:Int):IntVar = IntConst(a)
}

abstract class IntInvariant extends Invariant{
  def MyMin:Int
  def MyMax:Int
  implicit def toIntVar:IntVar = {
    val a = new IntVar(model,MyMin,MyMax,0,this.getClass.getSimpleName)
    a <== this //ca invoque setOutputVar en fait.
    a
  }

  /**this method is called by the output variable
   * basically, the invariant does not know what is its output variable at creation time.
   * if this is an issue, you can always create an output variable internally,
   * and implement this method with en identity invariant.
   * see [[oscar.cbls.invariants.core.computation.IdentityInt]] and [[oscar.cbls.invariants.core.computation.IdentityIntSet]]
   * @param v the variable that is the output variable of the invariant.
   */
  def setOutputVar(v:IntVar)
}

object IntInvariant{
  implicit def toIntVar(i:IntInvariant):IntVar = i.toIntVar
}

abstract class IntSetInvariant extends Invariant{
  def MyMin:Int
  def MyMax:Int
  implicit def toIntSetVar:IntSetVar = {
    val a = new IntSetVar(model,MyMin,MyMax,this.getClass.getSimpleName,SortedSet.empty)
    a <== this //the variable calls setoutputVar
    a
  }

  /**this method is called by the output variable
   * basically, the invariant does not know what is its output variable at creation time.
   * if this is an issue, you can always create an output variable internally,
   * and implement this method with en identity invariant.
   * see [[oscar.cbls.invariants.core.computation.IdentityInt]] and [[oscar.cbls.invariants.core.computation.IdentityIntSet]]
   * @param v the variable that is the output variable of the invariant.
   */
  def setOutputVar(v:IntSetVar)
}

object IntSetInvariant{
  implicit def toIntSetVar(i:IntSetInvariant):IntSetVar = i.toIntSetVar
}

case class IdentityInt(v:IntVar) extends IntInvariant {
  var output:IntVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def MyMax = v.MaxVal
  def MyMin = v.MinVal

  override def checkInternals(){
    assert(output.getValue(true) == v.getValue())
  }

  override def setOutputVar(vv:IntVar){
    output = vv
    output.setDefiningInvariant(this)
    output := v.getValue()
  }

  override def notifyIntChanged(v:IntVar,i:Int,OldVal:Int,NewVal:Int){
    assert(v == this.v)
    //ici, on propage tout de suite, c'est les variables qui font le stop and go.
    output := NewVal
  }
}

case class IdentityIntSet(v:IntSetVar) extends IntSetInvariant{

  var output:IntSetVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  val MyMin = v.getMinVal
  val MyMax = v.getMaxVal

  override def checkInternals(){
    assert(output.getValue(true).intersect(v.getValue()).size == v.getValue().size)
  }

  override def setOutputVar(vv:IntSetVar){
    output = vv
    output.setDefiningInvariant(this)
    output := v.value
  }

  override def notifyInsertOn(v:IntSetVar,value:Int){
    assert(v == this.v)
    output.insertValue(value)
  }

  override def notifyDeleteOn(v:IntSetVar,value:Int){
    assert(v == this.v)
    output.deleteValue(value)
  }
}

case class Singleton(v:IntVar) extends IntSetInvariant  {

  var output:IntSetVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def MyMin=v.MinVal
  def MyMax = v.MaxVal

  override def checkInternals(){
    assert(output.getValue(true).size == 1)
    assert(output.getValue(true).head == v.getValue())
  }

  override def setOutputVar(vv:IntSetVar){
    output = vv
    output.setValue(SortedSet(v.getValue()))
  }

  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    assert(v == this.v)
    //ici, on propage tout de suite, c'est les variables qui font le stop and go.
    output.deleteValue(OldVal)
    output.insertValue(NewVal)
  }
}
