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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.invariants.core.computation

import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.core.propagation._
import language.implicitConversions

/**This class contains the invariants and variables
  * They are all modelled as propagation Elements, which are handled by the inherited
  * [[oscar.cbls.invariants.core.propagation.PropagationStructure]] class.
  *
  * @param verbose requires that the propagation structure prints a trace of what it is doing.
  *                All prints are preceded by ''PropagationStruture''
  * @param checker specifies that once propagation is finished,
  *                it must call the checkInternals method on all propagation elements.
  * @param noCycle is to be set to true only if the static dependency graph between propagation elements has no cycles.
  *                If unsure, set to false, the engine will discover it by itself.
  *                See also method isAcyclic to query a propagation structure.
  * @param topologicalSort set to true if you want to use topological sort, to false for layered sort (layered is faster)
  * @param propagateOnToString set to true if a toString triggers a propagation, to false otherwise.
  *                            Set to false only for deep debugging
  * @param sortScc true if SCC should be sorted, false otherwise. Set to true, unless you know what your are doing.
  *                Setting to false might provide a speedup (eg in routing problems),
  *                but propagation will not be single pass on SCC anymore
  */
case class Store(override val verbose:Boolean = false,
                 override val checker:Option[Checker] = None,
                 override val noCycle:Boolean = true,
                 override val topologicalSort:Boolean = false,
                 val propagateOnToString:Boolean = true,
                 override val sortScc:Boolean = true)
  extends PropagationStructure(verbose,checker,noCycle,topologicalSort, sortScc)
  with Bulker with StorageUtilityManager{

  assert({println("You are using a CBLS store with asserts activated. It makes the engine slower. Recompile it with -Xdisable-assertions"); true})

  private var Variables:List[Variable] = List.empty
  private var Invariants:List[Invariant] = List.empty
  private var PropagationElements:List[PropagationElement] = List.empty
  private var Closed:Boolean=false

  def isClosed = Closed

  private var privateInputVariables:List[Variable] = null;

  def inputVariables():List[Variable] = {
    if(privateInputVariables == null){
      privateInputVariables  = List.empty
      for (v:Variable <- Variables if v.isInputVariable){
        privateInputVariables = v :: privateInputVariables
      }
    }
    privateInputVariables
  }
  /**To save the current value of the variables registered in the model
    * @param inputOnly if set to true (as by default) the solution will only contain the variables that are not derived through an invariant
    */
  def solution(inputOnly:Boolean = true):Solution = {
    var assignationInt:SortedMap[CBLSIntVar,Int] = SortedMap.empty
    var assignationIntSet:SortedMap[CBLSSetVar,SortedSet[Int]] = SortedMap.empty

    val VariablesToSave = if(inputOnly) {
      inputVariables()
    }else Variables

    for (v:Variable <- VariablesToSave){
      v match {
        case i:CBLSIntVar =>
          assignationInt += ((i, i.value))
        case s:CBLSSetVar =>
          assignationIntSet += ((s, s.value))
      }
    }
    Solution(assignationInt,assignationIntSet,this)
  }

  /**this is to be used as a backtracking point in a search engine
    * you can only save variables that are not controlled*/
  def saveValues(vars:Variable*):Snapshot = {
    var assignationInt:List[(CBLSIntVar,Int)] = List.empty
    var assignationIntSet:List[(CBLSSetVar,SortedSet[Int])] = List.empty
    for (v:Variable <- vars if v.isInputVariable){
      if(v.isInstanceOf[CBLSIntVar]){
        assignationInt = ((v.asInstanceOf[CBLSIntVar], v.asInstanceOf[CBLSIntVar].getValue(true))) :: assignationInt
      }else if(v.isInstanceOf[CBLSSetVar]){
        assignationIntSet = ((v.asInstanceOf[CBLSSetVar], v.asInstanceOf[CBLSSetVar].getValue(true))) :: assignationIntSet
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
    for((intsetvar,intset) <- s.assignationIntSet if intsetvar.isInputVariable){
      intsetvar := intset
    }
    for((intvar,int) <- s.assignationInt if intvar.isInputVariable){
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
    for((intsetvar,intset) <- s.assignationIntSet if intsetvar.isInputVariable){
      intsetvar := intset
    }
    for((intvar,int) <- s.assignationInt if intvar.isInputVariable){
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

  var toCallBeforeClose:List[(()=>Unit)] = List.empty

  def addToCallBeforeClose(toCallBeforeCloseProc : (()=>Unit)){
    toCallBeforeClose = (toCallBeforeCloseProc) :: toCallBeforeClose
  }


  protected def performCallsBeforeClose() {
    for (p <- toCallBeforeClose) p()
    toCallBeforeClose = List.empty
  }

  /**calls this when you have declared all your invariants and variables.
    * This must be called before any query and update can be made on the model,
    * and after all the invariants and variables have been declared.
   * @param DropStaticGraph true if you want to drop the static propagation graph to free memory. It takes little time
   */
  def close(DropStaticGraph: Boolean = true){
    assert(!Closed, "cannot close a model twice")
    performCallsBeforeClose()
    setupPropagationStructure(DropStaticGraph)
    killBulker() //we won't create any new model artifacts, thus we can kill the bulker and free its memory
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
  def toStringInputOnly = Variables.filter(v => v.isInputVariable).toString()

  //returns the set of source variable that define this one.
  // This exploration procedure explores passed dynamic invariants,
  // but it over-estimates the set of source variables over dynamic invariants, as it uses the static graph.
  def getSourceVariables(v:Variable):SortedSet[Variable] = {
    var ToExplore: List[PropagationElement] = List(v)
    var SourceVariables:SortedSet[Variable] = SortedSet.empty[Variable]
    while(!ToExplore.isEmpty){
      val head = ToExplore.head
      ToExplore = ToExplore.tail
      if(head.isInstanceOf[Variable]){
        val v:Variable = head.asInstanceOf[Variable]
        if(!SourceVariables.contains(v)){
          SourceVariables += v
          for(listened <- v.getStaticallyListenedElements)
            ToExplore = listened :: ToExplore
        }
        //TODO: keep a set of the explored invariants, to speed up this thing?
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

case class Snapshot(assignationInt:List[(CBLSIntVar, Int)],
                    assignationIntSet:List[(CBLSSetVar,SortedSet[Int])],
                    model:Store)

/**This class contains a solution. It can be generated by a model, to store the state of the search, and restored.
  * it remains linked to the model, as it maintains references to the variables declared in the model.
  * you cannot pass it over a network connection for instance.
  * see methods getSolution and restoreSolution in [[oscar.cbls.invariants.core.computation.Store]]
  */
case class Solution(assignationInt:SortedMap[CBLSIntVar,Int],
                    assignationIntSet:SortedMap[CBLSSetVar,SortedSet[Int]],
                    model:Store){

  /**to get the value of an IntVar in the saved solution*/
  def getSnapshot(v:CBLSIntVar):Int = assignationInt(v)

  /**to get the value of an IntSetVar in the saved solution*/
  def getSnapshot(v:CBLSSetVar):SortedSet[Int] = assignationIntSet(v)

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
abstract class Invariant extends PropagationElement{
  var model:Store = null

  def getPropagationStructure = this.model

  final def preFinishInitialization(model:Store = null):Store = {
    if (this.model== null){
      if (model == null){
        this.model = InvariantHelper.findModel(getStaticallyListenedElements)
      }else{
        this.model = model //assert(model == InvariantHelper.FindModel(getStaticallyListenedElements()))
      }
    }
    this.model
  }

  /**Must be called by all invariant after they complete their initialization
    * that is: before they get their output variable.
    * This performs some registration to the model, which is discovered by exploring the variables that are statically registered to the model
    * no more variable can be registered statically after this method has been called.
    * @param model; if specified, it only checks that the model is coherent, and registers to it for the ordering
    */
  final def finishInitialization(model:Store = null){
    preFinishInitialization(model)
    if (this.model!= null){
      uniqueID = this.model.registerInvariant(this)
    }else{
      uniqueID = -1
    }
  }

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
   * let be i, a position in the array, the index used for dynamic dependency registration is i+offset
   * @param v the variable that are registered
   * @param offset and offset applied to the position in the array when registering the dynamic dependency
   */
  def registerStaticAndDynamicDependencyArrayIndex[T <: Variable](v:Array[T],offset:Int = 0):Array[KeyForElementRemoval] = {
    Array.tabulate(v.size)((i:Int) => {
      registerStaticDependency(v(i))
      registerDynamicDependency(v(i),i + offset)
    })
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
  def notifyIntChangedAny(v:CBLSIntVar,i:Any,OldVal:Int,NewVal:Int){notifyIntChanged(v,i.asInstanceOf[Int], OldVal,NewVal)}

  def notifyIntChanged(v:CBLSIntVar,i:Int,OldVal:Int,NewVal:Int){notifyIntChanged(v,OldVal,NewVal)}

  def notifyIntChanged(v:CBLSIntVar,OldVal:Int,NewVal:Int){}

  def notifyInsertOnAny(v:CBLSSetVar,i:Any,value:Int){notifyInsertOn(v,i.asInstanceOf[Int],value)}

  def notifyInsertOn(v:CBLSSetVar,i:Int,value:Int){notifyInsertOn(v,value)}

  def notifyInsertOn(v:CBLSSetVar,value:Int){}

  def notifyDeleteOnAny(v:CBLSSetVar,i:Any,value:Int){notifyDeleteOn(v,i.asInstanceOf[Int],value)}

  def notifyDeleteOn(v:CBLSSetVar,i:Int,value:Int){notifyDeleteOn(v,value)}

  def notifyDeleteOn(v:CBLSSetVar,value:Int){}

  /**To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c:Checker){c.check(false, Some("DEFAULT EMPTY CHECK " + this.toString() + ".checkInternals"))}

  def getDotNode = "[label = \"" + this.getClass.getSimpleName + "\" shape = box]"
}

object InvariantHelper{
  /**this is useful to find the model out of a set of propagation elements.
    *
    * @param i some propagation elements, typically, variables listened by some invariants
    * @return the model that the invariant belongs to
    */
  def findModel(i:Iterable[PropagationElement]):Store={
    i.foreach(e => {
      if (e.isInstanceOf[Variable]){
        val m = e.asInstanceOf[Variable].model
        if (m != null){
          return m
        }
      }
    })
    null
  }

  def getMinMaxBounds(variables:Iterable[CBLSIntVar]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.maxVal) MyMax = v.maxVal
      if (MyMin > v.minVal) MyMin = v.minVal
    }
    (MyMin, MyMax)
  }
  def getMinMaxBoundsIntSetVar(variables:Iterable[CBLSSetVar]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.getMaxVal) MyMax = v.getMaxVal
      if (MyMin > v.getMinVal) MyMin = v.getMinVal
    }
    (MyMin, MyMax)
  }

  def arrayToString[T<:Variable](a:Array[T]):String = {
    "[" + a.toList.mkString(",")+"]"
  }

}

/**This is the base class for variable. A variable is a propagation element that holds some value.
  * Variables have an associated model, to which they register as soon as they are created. Variables also have a name,
  * which is used solely for printing models.
  */
abstract class Variable(val model:Store, n:String = null) extends PropagationElement with DistributedStorageUtility{
  uniqueID = if (model == null) -1 else model.registerVariable(this)
  val name = Option(n) getOrElse (s"Var_$uniqueID")
  def getPropagationStructure = this.model

  protected var definingInvariant:Invariant = null

  def setDefiningInvariant(i:Invariant) {
    assert(i.model == model || i.model == null,"i.model == null:" + (i.model == null) + " i.model == model:" + (i.model == model) + " model == null:" + (model == null))
    if(definingInvariant == null){
      definingInvariant = i
      registerStaticallyListenedElement(i)
      registerDynamicallyListenedElement(i,0)
    }else{
      throw new Exception("variable [" + name + "] cannot have more than one controling invariant, already has " + definingInvariant)
    }
  }

  def isControlledVariable:Boolean = definingInvariant != null
  def isInputVariable:Boolean = definingInvariant == null

  /**this method s to be called by any method that internally modifies the value of the variable
    * it schedules the variable for propagation, and performs a basic check of the identify of the executing invariant*/
  def notifyChanged(){
    //modifier le test.
    if (this.model == null ||(!this.model.isClosed && this.getDynamicallyListeningElements.isEmpty)){
      performPropagation()
    }else{
      assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this + "] affected by non-controlling invariant")
      scheduleForPropagation()
    }
  }

  /** this method is a toString that does not trigger a propagation.
    * use this when debugguing your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate:String

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

  def apply(v:Variable,
            action: =>Unit):Event = {
    val toreturn = new Event(v,null,null)
    toreturn.setAction(() => action)
    //    if (intaction != null) toreturn.setIntAction(intaction)
    //   if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    toreturn
  }

  def apply(v:Variable,
            action: =>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,null,ModifiedVars)
    toreturn.setAction(() => action)
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

  private var action: (()=>Unit)=null
  private var actionIntParam: (Int=>Unit) = null
  private var actionIntSetParam: (SortedSet[Int] => Unit) = null

  private var oldIntv = 0
  private var oldIntSetv:SortedSet[Int] = SortedSet.empty
  private var oldIntw = 0
  private var oldIntSetw:SortedSet[Int] = SortedSet.empty

  private var intintaction: ((Int,Int) => Unit) = null
  private var intsetintsetaction:((SortedSet[Int],SortedSet[Int]) => Unit) = null
  private var intsetintaction:((SortedSet[Int],Int) => Unit) = null
  private var intintsetaction:((Int,SortedSet[Int]) => Unit) = null

  def setAction(action: ()=>Unit){
    this.action = action
  }
  def setIntAction(action: Int=>Unit){
    this.actionIntParam = action
    oldIntv = v.asInstanceOf[CBLSIntVar].value
  }
  def setIntSetAction(action: SortedSet[Int] => Unit){
    this.actionIntSetParam = action
    oldIntSetv = v.asInstanceOf[CBLSSetVar].value
  }

  def setintintaction(intintaction: (Int,Int)=>Unit){
    this.intintaction = intintaction
    this.oldIntv = v.asInstanceOf[CBLSIntVar].value
    this.oldIntw = w.asInstanceOf[CBLSIntVar].value
  }

  def setintsetintsetaction(intsetintsetaction:(SortedSet[Int],SortedSet[Int]) => Unit){
    this.intsetintsetaction = intsetintsetaction
    this.oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    this.oldIntSetw = w.asInstanceOf[CBLSSetVar].value
  }

  def setintsetintaction(intsetintaction:(SortedSet[Int],Int) => Unit){
    this.intsetintaction = intsetintaction
    this.oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    this.oldIntw = w.asInstanceOf[CBLSIntVar].value
  }

  def setintintsetaction(intintsetaction:(Int,SortedSet[Int]) => Unit){
    this.intintsetaction = intintsetaction
    this.oldIntv = v.asInstanceOf[CBLSIntVar].value
    this.oldIntSetw = w.asInstanceOf[CBLSSetVar].value
  }

  registerStaticAndDynamicDependency(v)
  if(w!=null)  registerStaticAndDynamicDependency(w)
  finishInitialization()
  if (ModifiedVars != null)
    for(variable <- ModifiedVars){variable.setDefiningInvariant(this)}

  override def notifyIntChanged(v:CBLSIntVar,i:Int,OldVal:Int,NewVal:Int){scheduleForPropagation()}
  override def notifyInsertOn(v:CBLSSetVar,i:Int,value:Int){scheduleForPropagation()}
  override def notifyDeleteOn(v:CBLSSetVar,i:Int,value:Int){scheduleForPropagation()}

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
      oldIntv = v.asInstanceOf[CBLSIntVar].value
    }
    if (actionIntSetParam != null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    }
    if(intintaction!=null){
      oldIntv = v.asInstanceOf[CBLSIntVar].value
      oldIntw = w.asInstanceOf[CBLSIntVar].value
    }
    if (intsetintsetaction!=null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
      oldIntSetw = w.asInstanceOf[CBLSSetVar].value
    }
    if (intsetintaction!=null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
      oldIntw = w.asInstanceOf[CBLSIntVar].value
    }
    if (intintsetaction != null){
      oldIntv = v.asInstanceOf[CBLSIntVar].value
      oldIntSetw = w.asInstanceOf[CBLSSetVar].value
    }
  }

  override def checkInternals(c: Checker) = c.check(true, Some("Event.checkInternals"))
}

/**An IntVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is integer.
  *
  * @param model is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSIntConst]]
  * @param domain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param Value is the value of the variable
  * @param n is the name of the variable, used for pretty printing only
  */
class CBLSIntVar(model: Store, val domain: Range, private var Value: Int, n: String = null)
  extends Variable(model, n) {

  private var OldValue = Value

  def inDomain(v: Int) = domain.contains(v)
  def minVal = domain.start
  // if the range was specified using until max should be max -1
  def maxVal = if(domain.isInclusive) domain.end else domain.end - 1

  override def toString = if(model.propagateOnToString) s"$name:=$value" else s"$name:=$Value"
  override def toStringNoPropagate = s"$name:=$Value" //value

  def setValue(v:Int){
    if (v != Value){
      //TODO: disable assert while domain of invariant are buggy, this assert is needed in UNIT TEST.
      // (-Xdisable-assertions as argument of scala compiler)
      // or comment this assert and use it only to throw unit test while domain bugs.
      /*
      assert(inDomain(v),print("Assertion False : variable ["+this+"] is not in his domain \n" +
          "domain : ["+MinVal+ ";"+MaxVal+"]\n" +
           "new value :"+ v +"\n" ))
           */
      Value = v
      notifyChanged()
    }
  }

  def value: Int = getValue()

  def getValue(NewValue: Boolean = false): Int = {
    if(NewValue){
      assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
        + "] queried for latest val by non-controlling invariant")
      Value
    } else{
      if (model == null) return Value
      if (definingInvariant == null && !model.Propagating) return Value
      model.propagate(this)
      OldValue
    }
  }

  override def performPropagation(){
    if(OldValue!=Value){
      val old=OldValue
      OldValue=Value
      for (e:((PropagationElement,Any)) <- getDynamicallyListeningElements){ //TODO: here should come some postponed stuff as well
      val inv:Invariant = e._1.asInstanceOf[Invariant]
        assert({this.model.NotifiedInvariant=inv; true})
        inv.notifyIntChangedAny(this,e._2,old,Value)
        assert({this.model.NotifiedInvariant=null; true})
      }
    }
  }

  def :=(v: Int) {
    setValue(v)
  }

  def :+=(v: Int) {
    setValue(v + getValue(true))
  }

  def :*=(v: Int) {
    setValue(v * getValue(true))
  }

  def :-=(v:Int) {
    setValue(getValue(true) - v)
  }

  /** increments the variable by one
    */
  def ++ {
    setValue(1 + getValue(true))
  }

  /**this operator swaps the value of two IntVar*/
  def :=:(v:CBLSIntVar){
    val a:Int = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two IntVar*/
  def swap(v: CBLSIntVar) {
    this :=: v
  }

  def <==(i: IntInvariant) {i.setOutputVar(this)}
  def <==(i: CBLSIntVar) {this <== i.getClone}

  def getClone:IdentityInt = IdentityInt(this)

  override def checkInternals(c:Checker){
    c.check(OldValue == Value)
  }

  def getDotNode = "[label = \"IntVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object CBLSIntVar{

  def apply(r:Range, v:Int, name:String)(implicit s:Store) = new CBLSIntVar(s,r,v,name)
  def apply(r:Range, v:Int)(implicit s:Store) = new CBLSIntVar(s,r,v,"")

  def apply(model: Store, minVal:Int, maxVal:Int, value:Int , name:String) = {
    require(minVal <= maxVal, "the minVal must be less than or equal to the maxVal of the domain minVal:" + minVal + " maxVal:" + maxVal)
    new CBLSIntVar(model,(minVal to maxVal), value, name)
  }

  def apply(model: Store, domain: Range, value:Int, name:String) = {
    require(!domain.isEmpty, "the domain supplied must be a valid increasing interval")
    new CBLSIntVar(model, domain, value, name)
  }

  def apply(model: Store, value:Int = 0, name:String = "") = {
    val domain = Int.MinValue to Int.MaxValue
    new CBLSIntVar(model, domain, value, name)
  }

  implicit val ord:Ordering[CBLSIntVar] = new Ordering[CBLSIntVar]{
    def compare(o1: CBLSIntVar, o2: CBLSIntVar) = o1.compare(o2)
  }

  implicit def int2IntVar(a:Int):CBLSIntVar = CBLSIntConst(a)

  implicit def toFunction(i:CBLSIntVar):()=>Int = () => i.value

  implicit def namedIntConst(valAndName:(Int,String)):CBLSIntVar = CBLSIntConst(valAndName._1, null, valAndName._2)
}

/**
 * An IntConst is an [[oscar.cbls.invariants.core.computation.CBLSIntVar]] that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param ConstValue: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 */
case class CBLSIntConst(ConstValue:Int, override val model:Store = null, override val name: String = null)
  extends CBLSIntVar(model, (ConstValue to ConstValue), ConstValue, toString){
  override def getValue(NewValue:Boolean=false):Int = ConstValue //pour pas avoir de propagation
  override def toString:String = if (name == null) "IntConst("+ ConstValue + ")" else name + ":=" + ConstValue
}

/**An IntSetVar is a variable managed by the [[oscar.cbls.invariants.core.computation.Store]] whose type is set of integer.
  * @param model is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.invariants.core.computation.CBLSSetConst]]
  * @param MinVal is the minimum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays
  * @param MaxVal is the maximum value of integers included in this set. Some invariants exploit this value to declare fixed size arrays.
  * @param Value is the value of the variable
  * @param name is the name of the variable, used for pretty printing only
  * @author renaud.delandtsheer@cetic.be
  * */
class CBLSSetVar(override val model:Store,
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

  private def Perform(){
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

  override def checkInternals(c:Checker){
    assert(this.definingInvariant == null || OldValue.intersect(Value).size == Value.size,
      "internal error: " + "Value: " + Value + " OldValue: " + OldValue)
  }

  override def toString:String = name + ":={" + (if(model.propagateOnToString) value else Value).mkString(",") + "}"

  def valueString:String = "{" + value.mkString(",") + "}"


  /** this method is a toString that does not trigger a propagation.
    * use this when debugguing your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate: String = name + ":={" + Value.foldLeft("")(
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
          //We simply replay the history. If some backtrack was performed, it is suboptimal
          // eg: if something was propagated to this during a neighbourhood exploration not involving this var
          if (inserted){
            //inserted
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
      assert(getModel.checkExecutingInvariantOK(definingInvariant),
        "variable [" + this + "] queried for latest val by non-controlling invariant")
      Value
    }else{
      if (model == null) return Value
      if (definingInvariant == null && !model.Propagating) return Value
      model.propagate(this)
      Perform()
      OldValue
    }
  }

  /**Use this to specify that the IntSetVar is the output of the IntSetInvariant*/
  def <==(i:SetInvariant){i.setOutputVar(this)}
  def <==(i: CBLSSetVar) {this <== IdentitySet(i)}

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  def :=(v:SortedSet[Int]) {setValue(v)}

  def :+=(i:Int) {this.insertValue(i)}
  def :-=(i:Int) {this.deleteValue(i)}

  def getDotNode = "[label = \"IntSetVar(" + name + ")\" shape = oval color = " + getDotColor + "]"
}

object CBLSSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  def apply(r:Range = Int.MinValue to Int.MaxValue, v:Iterable[Int] = List.empty, name:String="")(implicit s:Store) = {
    val emptySet:SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, r.start, r.end,name, emptySet ++ v)
  }

  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }

  implicit def intSet2IntSetVar(a:SortedSet[Int]):CBLSSetVar = CBLSSetConst(a)

  implicit def toFunction(s:CBLSSetVar):()=>SortedSet[Int] = () => s.value
}

/**
 * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param ConstValue: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 * */
case class CBLSSetConst(ConstValue:SortedSet[Int],override val model:Store = null)
  extends CBLSSetVar(model
    ,if(ConstValue.isEmpty) Int.MinValue else ConstValue.min
    ,if(ConstValue.isEmpty) Int.MaxValue else ConstValue.max
    ,toString,ConstValue){
  override def getValue(NewValue:Boolean=false):SortedSet[Int] = ConstValue //pour pas avoir de propagation
  override def toString:String = "IntSetConst{" + ConstValue.mkString(",") + "}"
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class IntInvariant extends Invariant{
  def myMin:Int
  def myMax:Int
  implicit def toIntVar:CBLSIntVar = toIntVar(this.getClass.getSimpleName)

  def toIntVar(name:String):CBLSIntVar = {
    val a = new CBLSIntVar(model, (myMin to myMax), 0, name)
    a <== this //ca invoque setOutputVar en fait.
    a
  }


  /**this method is called by the output variable
    * basically, the invariant does not know what is its output variable at creation time.
    * if this is an issue, you can always create an output variable internally,
    * and implement this method with en identity invariant.
    * see [[oscar.cbls.invariants.core.computation.IdentityInt]] and [[oscar.cbls.invariants.core.computation.IdentitySet]]
    * @param v the variable that is the output variable of the invariant.
    */
  def setOutputVar(v:CBLSIntVar)
}

object IntInvariant{
  implicit def toIntVar(i:IntInvariant):CBLSIntVar = i.toIntVar
}

/*
* @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant extends Invariant{
  def myMin:Int
  def myMax:Int
  implicit def toSetVar:CBLSSetVar = {
    val a = new CBLSSetVar(model,myMin,myMax,this.getClass.getSimpleName,SortedSet.empty)
    a <== this //the variable calls setOutputVar
    a
  }

  def toSetVar(name:String):CBLSSetVar = {
    val a = new CBLSSetVar(model,myMin,myMax,name,SortedSet.empty)
    a <== this //the variable calls setoutputVar
    a
  }

  @deprecated("use toSetVar instead", "1.1")
  def toIntSetVar:CBLSSetVar = toSetVar

  /**this method is called by the output variable
    * basically, the invariant does not know what is its output variable at creation time.
    * if this is an issue, you can always create an output variable internally,
    * and implement this method with en identity invariant.
    * see [[oscar.cbls.invariants.core.computation.IdentityInt]] and [[oscar.cbls.invariants.core.computation.IdentitySet]]
    * @param v the variable that is the output variable of the invariant.
    */
  def setOutputVar(v:CBLSSetVar)
}

object SetInvariant{
  implicit def toIntSetVar(i:SetInvariant):CBLSSetVar = i.toSetVar
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  * @param v
  */
case class IdentityInt(v:CBLSIntVar) extends IntInvariant {
  var output:CBLSIntVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def myMax = v.maxVal
  def myMin = v.minVal

  override def checkInternals(c:Checker){
    c.check(output.getValue(true) == v.value)
  }

  override def setOutputVar(vv:CBLSIntVar){
    output = vv
    output.setDefiningInvariant(this)
    output := v.value
  }

  override def notifyIntChanged(v:CBLSIntVar,i:Int,OldVal:Int,NewVal:Int){
    assert(v == this.v)
    //ici, on propage tout de suite, c'est les variables qui font le stop and go.
    output := NewVal
  }


}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  * @param v
  */
case class IdentitySet(v:CBLSSetVar) extends SetInvariant{

  var output:CBLSSetVar = null
  registerStaticAndDynamicDependency(v)
  finishInitialization()

  val myMin = v.getMinVal
  val myMax = v.getMaxVal

  override def checkInternals(c:Checker){
    c.check(output.getValue(true).intersect(v.value).size == v.value.size)
  }

  override def setOutputVar(vv:CBLSSetVar){
    output = vv
    output.setDefiningInvariant(this)
    output := v.value
  }

  override def notifyInsertOn(v:CBLSSetVar,value:Int){
    assert(v == this.v)
    output.insertValue(value)
  }

  override def notifyDeleteOn(v:CBLSSetVar,value:Int){
    assert(v == this.v)
    output.deleteValue(value)
  }
}


