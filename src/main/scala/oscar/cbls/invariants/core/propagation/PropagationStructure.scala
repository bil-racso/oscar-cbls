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

package oscar.cbls.invariants.core.propagation

import oscar.cbls.invariants.core.algo.dag._;
import oscar.cbls.invariants.core.algo.tarjan._
import oscar.cbls.invariants.core.algo.dll._;
import collection.immutable.SortedMap
import collection.mutable.Queue
import oscar.cbls.invariants.core.algo.heap.{AggregatedBinomialHeap, AbstractHeap, BinomialHeap}
;

/**
 * This class manages propagation among propagation elements.
 *
 * This class is intended to be extended, and the overriding class must implement
 * the method getPropagationElements that returns the propagation elements to be considered
 * Each propagation element has a UniqueID. Those should be assigned continuously starting from 0.
 *
 * It is to be used as follows: once the set of propagation elements is stabilized,
 * one must call setupPropagationStructure, which will built the necessary internal data structure
 * propagation are triggered by calling the propagate method.
 * additionally, before calling setupPropagationStructure, the method registerForPartialPropagation can be
 * called to specify propagation elements that might require lazy propagation.
 *
 *
 *  Two debug mechanisms are provided: trace printing and debug mode.
 *
 *  A trace printing is provided; the propagation structure prints a trace of what it is propagating.
 *  This is activated by the Verbose parameter. All prints are preceded by ''PropagationStruture''
 *  This an be useful when checking the behavior of partial propagation.
 *
 *  A self-check method is called by the propagation structure after propagation is performed.
 *  This is activated by the Checker parameter.
 *  You should ensure that Asteroid is compiled with assert activated if you are using the debug mode.
 *  It will considerably slow down Asteroid, as other checks are implemented in the base modules.
 *
 *  Also, although this propagation structure is intended to support acyclic graph
 *  for the static dependency graph, you can deactivate the associated mechanism
 *  by setting the IsAcyclic graph to true.
 *  If unsure, set to false (or do not set; it is false by default),
 *  the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
 *
 * @param Verbose requires that the propagation structure prints a trace of what it is doing.
 * @param Checker: set a Some[Checker] top check all internal properties of invariants after propagation, set to None for regular execution
 * @param NoCycle is to be set to true only if the static dependency graph is acyclic.
 * @param TopologicalSort if true, use topological sort, false, use distance to input, and associated faster heap data structure
 */
abstract class PropagationStructure(val Verbose: Boolean, val Checker:Option[checker] = None, val NoCycle: Boolean, val TopologicalSort:Boolean) extends StorageUtilityManager{
  //priority queue is ordered, first on propagation planning list, second on DAG.

  /**This method is to be overridden and is expected to return the propagation elements
   * on which the propagation structure will reason.
   * The method is expected to return consistent result once the setupPropagationStructure method is called
   */
  def getPropagationElements: Iterable[PropagationElement]

  /**This method is to be overridden and is expected to return the maximal value of the UniqueID
   * of the propagation elements
   * The method is expected to return consistent result once the setupPropagationStructure method is called
   */
  private var MaxID: Int = -1

  def getMaxID = MaxID

  def GetNextID(): Int = {
    MaxID += 1
    MaxID
  }

  private var acyclic: Boolean = false;

  /**@return true if the propagation structure consider that his graph is acyclic, false otherwise.
   * call this after the call to setupPropagationStructure
   * If the propagation structure has been created with NoCycle set to true, this will return true
   */
  def isAcyclic: Boolean = acyclic;

  /**return the summed stalls of all SCC.
   * A stall is when the SCC is unable to maintain the topological sort incrementally,
   * and must recompute it from scratch
   * this happens when dependencies are modified with transient cycles
   * @return the summed number of stalls for all the SCC
   */
  def getStalls = StrognlyConnexComponentsList.foldLeft(0)((acc,scc) => acc + scc.getStalls)

  private var StrognlyConnexComponentsList: List[StronglyConnectedComponent] = List.empty

  /**To call when one has defined all the propagation elements on which propagation will ever be triggered.
   * It must be called before any propagation is triggered,
   * as it allows the propagation structure to build the necessary internal structures
   * @param DropStaticGraph if true, the propagation structure drops the static graph after setup.
   */
  protected def setupPropagationStructure(DropStaticGraph: Boolean) {

    if (Verbose) {
      println("PropagationStructure: closing propagation structure. Propagations structure includes: ")
      getPropagationElements.foreach(p => println("+ " + p))
      println("PropagationStructure: end propagations structure includes; size=" + getPropagationElements.size)
    }

    buildFastPropagationTracks()

    val StrognlyConnectedComponents: List[List[PropagationElement]] =
      if (NoCycle) {
        if (Verbose) {
          println("PropagationStructure: IsAcyclic flag; assuming acyclic static dependency graph ")
        }
        var toreturn: List[List[PropagationElement]] = List.empty
        for (n <- getPropagationElements) toreturn = List(n) :: toreturn
        toreturn
      } else {
        //identification des composantes connexes
        TarjanWithBigNodes.getStronlyConnexComponents[PropagationElement](
          getPropagationElements,
          p => p.getStaticallyListeningElements)
      }

    assert(getPropagationElements.size == StrognlyConnectedComponents.foldLeft(0)
      ((acc, component) => acc + component.size))

    //tri topologique sur les composantes fortement connexes
    acyclic = true;
    StrognlyConnexComponentsList = List.empty;
    val ClusteredPropagationComponents: List[PropagationElement] = StrognlyConnectedComponents.map(a => {
      if (a.tail.isEmpty) {
        a.head
      } else {
        acyclic = false;
        val c = new StronglyConnectedComponent(a, this, GetNextID())
        StrognlyConnexComponentsList = c :: StrognlyConnexComponentsList
        c
      }
    })

    //this performs the sort on Propagation Elements that do not belong to a strongly connected component,
    // plus the strongly connected components, considered as a single node. */
    var LayerCount = 0;
    if (TopologicalSort){
      computePositionsThroughTopologialSort(ClusteredPropagationComponents)
    }else{
      LayerCount = computePositionsthroughDistanceToInput(ClusteredPropagationComponents)+1
    }


    //puis, il faut initialiser le dag sort incremental pour toutes les composantes connexes non singleton.
    //il faut que le DAG fonctionne sur base des aretes reelles.
    //pour ce faire, il faut faire une premiere propagation
    //on ne passe pas encore en mode autosort, on le fait lors de la premiere propagation, juste apres la propagation en fait.
    //cette propagation est donc profondement inefficace puisqu'on pourrait boucler en cas de boucle.
    // l'idee est donc que tous les elements de propagation propagent lors de leur initialisation.

    //calculer les boundary avant la premiere propagation
    for (f <- ClusteredPropagationComponents) {
      f.determineBoundary()
    }

    if (TopologicalSort){
      ExecutionQueue = new BinomialHeap[PropagationElement](p => p.Position, ClusteredPropagationComponents.size)
    }else{
      ExecutionQueue = new AggregatedBinomialHeap[PropagationElement](p => p.Position, LayerCount)
    }

    Propagating = false
    PreviousPropagationTarget = null

    if (DropStaticGraph) dropStaticGraph()

    //variables are already able to propagate immediately before model close and if not monitored yet.

    ScheduledElements = List.empty
    for (e <- getPropagationElements) {
      e.rescheduleIfNeeded
    }
    for (scc <- StrognlyConnexComponentsList){
      scc.rescheduleIfNeeded()
    }
    //propagate() we do not propagate anymore here since the first query might require a partial propagation only
  }

  /**This computes the position of the clustered PE, that is: the SCC and the PE not belonging to an SCC*/
  private def computePositionsThroughTopologialSort(ClusteredPropagationComponents:List[PropagationElement]){
    if (Verbose) println("PropagationStructure: Positioning through topological sort")
    var Front: List[PropagationElement] = ClusteredPropagationComponents.filter(n => {n.setCounterToPrecedingCount();(n.Position == 0)})
    var Position = 0 //la position du prochain noeud place.
    while (!Front.isEmpty) {
      val n = Front.head
      Front = Front.tail
      n.Position = Position
      Position += 1
      Front = n.decrementSucceedingAndAccumulateFront(Front)
    }
    if (Position != ClusteredPropagationComponents.size) {
      if (NoCycle){
        throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
      }else{
        throw new Exception("internal bug")
      }
    }
  }

  /**This computes the position of the clustered PE based on distance to input,
   * that is: the SCC and the PE not belonging to an SCC
   * @return the max Position, knowing that the first is zero*/
  private def computePositionsthroughDistanceToInput(ClusteredPropagationComponents:List[PropagationElement]):Int = {
    if (Verbose) println("PropagationStructure: Positioning through layered sort")
    val Front:Queue[PropagationElement] =  new Queue[PropagationElement]()
    for (pe <- ClusteredPropagationComponents){
      pe.setCounterToPrecedingCount()
      if(pe.Position == 0) Front.enqueue(pe)
    }
    Front.enqueue(null) //null marker denotes when Position counter must be incremented
    var Position = 0 //la position du prochain noeud place.
    var Count = 0 //the number of PE
    var CountInLayer = 0

    while (true) {
      val n = Front.dequeue()
      if (n == null){
        if (Front.isEmpty){
          if (Verbose) println("PropagationStructure: Layer " + Position + " #Elements:" + CountInLayer)
          if (Count != ClusteredPropagationComponents.size) {
            if (NoCycle){
              throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
            }else{
              throw new Exception("internal bug")
            }
          }
          return Position+1
        }else{
          if (Verbose) println("PropagationStructure: Layer " + Position + " #Elements:" + CountInLayer)
          CountInLayer=0
          Position +=1
          Front.enqueue(null) //null marker denotes when Position counter must be incremented
        }
      }else{
        n.Position = Position
        Count +=1
        CountInLayer+=0
        for (pe <- n.decrementSucceedingAndAccumulateFront(List.empty)) Front.enqueue(pe)
      }
    }
    0 //never reached
  }

  def dropStaticGraph() {
    for (p <- getPropagationElements) p.dropStaticGraph()
  }

  private var ScheduledElements: List[PropagationElement] = List.empty
  private var ExecutionQueue: AbstractHeap[PropagationElement] = null
  private var FastPropagationTracks: SortedMap[PropagationElement, Array[Boolean]] =
    SortedMap.empty[PropagationElement, Array[Boolean]]

  /**to call before setupPropagationStructure to specify PropagationElements
   * on which one will invoque partial propagation
   */
  def registerForPartialPropagation(p: PropagationElement) {
    FastPropagationTracks += ((p, null))
  }

  private var PreviousPropagationTarget: PropagationElement = null

  /**triggers the propagation in the graph.
   * this method will do nothing if called before setupPropagationStructure
   * if UpTo set to a PropagationElement,
   * and provided it has been registered through the registerForPartialPropagation method,
   * the propagation will be partial, targeting this element.
   * @param UpTo: the optional target of partial propagation
   */
  final def propagate(UpTo: PropagationElement = null) {
    if (!Propagating) {
      if (UpTo != null) { //TODO: if nothing before, just propagate the element and stop this.
        val Track = FastPropagationTracks.getOrElse(UpTo, null)
        val SameAsBefore = (Track != null && (PreviousPropagationTarget == UpTo))
        propagateOnTrack(Track, SameAsBefore)
      } else {
        propagateOnTrack(null, false)
      }
      PreviousPropagationTarget = UpTo
    }
  }

  /**Builds and stores the partial propagation tracks*/
  private def buildFastPropagationTracks() {
    if (!FastPropagationTracks.isEmpty) {
      //calculer la reacheability sur le graphe statique par algo de Floyd Warshall
      //on prend les listening elements parce-que certains peuvent ne pas etre enregistres dans le modele
      // si ils sont en entree du graphe.
      val keys = FastPropagationTracks.keys
      for (n <- keys) {
        FastPropagationTracks += ((n, BuildFastPropagationtrack(n)))
      }
    }
  }

  /**Builds the partial propagation track for the specified target
   * @param target the propagation element for which we build the partial propagation track
   * @return an array of boolean: UniqueID => should the element with UniqueID be propagated for this target?
   */
  private def BuildFastPropagationtrack(target: PropagationElement): Array[Boolean] = {
    val Track: Array[Boolean] = new Array[Boolean](getMaxID + 1)
    for (i <- 0 to getMaxID) Track(i) = false

    var ToExplore: List[PropagationElement] = List(target)
    Track(target.UniqueID) = true

    while (!ToExplore.isEmpty) {
      val n = ToExplore.head
      ToExplore = ToExplore.tail
      for (nn <- n.getStaticallyListenedElements)
        if (nn.UniqueID != -1 && !Track(nn.UniqueID)) {
          ToExplore = nn :: ToExplore
          Track(nn.UniqueID) = true
        }
    }

    for (scc <- StrognlyConnexComponentsList) {
      Track(scc.UniqueID) = Track(scc.Elements.head.UniqueID)
    }
    Track
  }

  private var PostponedComponents: List[PropagationElement] = List.empty

  /**
   * performs a propagation on a propagation track
   * if propagation track is omitte, total propagation is performed
   * @param Track the propagation track, an array indices_of_propagation_element -> should it be propagated now
   * @param SameAsBefore the previous propagation was on the same track, so that the postponed element are still postponed
   */
  private def propagateOnTrack(Track: Array[Boolean], SameAsBefore: Boolean) {
    if (Propagating) return
    Propagating = true

    if (!SameAsBefore) {
      var NewPostponed: List[PropagationElement] = List.empty
      for (e: PropagationElement <- PostponedComponents) {
        if (Track == null || Track(e.UniqueID)) {
          ScheduledElements = e :: ScheduledElements
        } else {
          NewPostponed = e :: NewPostponed
        }
      }
      PostponedComponents = NewPostponed
    } //if it is SameAsBefore, we do not check whether the elements are in the track,
    // as they are postponed, they are not in it anyway
    //notice that for partial propagation, connex components cannot be partially propagated
    // because they are strognly connected over the static propagation graph.

    if (Verbose) {
      if (Track == null) println("PropagationStruture: start total propagation")
      else println("PropagationStruture: start partial propagation")
    }

    for (e: PropagationElement <- ScheduledElements) {
      if (Track == null || Track(e.UniqueID)) {
        ExecutionQueue.insert(e)
      } else {
        PostponedComponents = e :: PostponedComponents
      }
    }
    ScheduledElements = List.empty

    while (!ExecutionQueue.isEmpty) {
      ExecutionQueue.popFirst().propagate()
      for (e <- ScheduledElements) {
        if (Track == null || Track(e.UniqueID)) {
          ExecutionQueue.insert(e)
        } else {
          PostponedComponents = e :: PostponedComponents
        }
      }
      ScheduledElements = List.empty
    }

    if (Verbose) println("PropagationStruture: end propagation")

    if (Track == null) {
      Checker match{
        case Some(c) =>
          for (p <- getPropagationElements) {
            p.checkInternals(c)
          }
        case None =>
      }
    }
    Propagating = false
  }

  /**this method is used by propagationComponents to schedule themself for propagation. */
  def scheduleForPropagation(p: PropagationElement) {
    ScheduledElements = p :: ScheduledElements
  }

  /**this variable controls propagation.
   * initially true to avoid spurious propagation during the construction of the data structure;
   * set to false by setupPropagationStructure
   */
  private var Propagating: Boolean = true

  /**this variable is set by the propagation element to notify that they are propagating.
   * it is used to ensure that no propagation element perform illegal operation
   * such as writing a variable they do not control, etc)*/
  var PropagatingElement: PropagationElement = null

  /**returns the propagation element that is currently propagating.
   * it allows one to ensure that the propagating element behaves as declared in its dependencies
   */
  def getPropagatingElement: PropagationElement = PropagatingElement

  /**This dumps the propagation graphs in a dot format, for documentation purposes
   * Static graph should only be set if the static graph has not been dropped
   * @param StaticGraph adds the static graph as red arrows
   * @param DynamicGraph adds the dynamic graph as blue arrows
   * @return a string that contains the dot format
   **/
  def dumpToDot(StaticGraph: Boolean, DynamicGraph: Boolean, Target:PropagationElement = null): String = {
    var ToReturn = "digraph PropagationStructure {\n"
    ToReturn += "   rankdir=LR;\n"
    def nodeName(p: PropagationElement) = "node" + p.UniqueID

    if(!StaticGraph && !DynamicGraph)
      throw new Exception("you want to dump to dot, but none of the static and dynamic graphs")

    for (e <- getPropagationElements if e.component == null) {
      if (! (!StaticGraph && e.isInstanceOf[BulkPropagator]))
        ToReturn += "   " + nodeName(e) + e.getDotNode + "\n"
    }

    for (scc <- StrognlyConnexComponentsList){
      ToReturn += "   subgraph " + "cluster_"+nodeName(scc) + "{" + "\n"
      for (f <- scc.Elements) {
        ToReturn += "      " + nodeName(f) + f.getDotNode + "\n"
      }
      ToReturn += "   }" + "\n"
    }

    if (StaticGraph && DynamicGraph){
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.UniqueID != -1) {
          if (e.getDeterminingElements.exists(p => p==f)){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else if (e.getDynamicallyListenedElements.exists(p => p==f)){
            //in static and dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }else{
            //only in static graph
            if(this.isAcyclic){
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
            }else{
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted constraint=false]" + "\n"
            }
          }
        }
        for (f <- e.getDynamicallyListenedElements if f.UniqueID != -1) {
          if (!e.getStaticallyListenedElements.exists(p => p==f)){
            //in dynamic graph and not in static one because of bulking
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }else if (StaticGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.UniqueID != -1) {
          ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
        }
      }
    }else if (DynamicGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getDynamicallyListenedElements if f.UniqueID != -1) {
          if (e.getDeterminingElements.exists(p => p==f)){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else{
            //in dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }
    ToReturn + "}\n"
  }

  /**Builds a dictionary to store data related to the PE.
   * the dictionary is O(1), based on an array.
   * It only works on PE that are registered to this structure.
   * The storage is not initialized, call the initialize to set it to some conventional value. 
   * @tparam T the type stored in the data structure
   * @return a dictionary over the PE that are registered in the propagation structure.
   */
  def getNodeStorage[T](implicit X:Manifest[T]):NodeDictionary[T] = new NodeDictionary[T](this.MaxID)
}

/**This is a O(1) dictionary for propagation elements.
 * It is based on an array, and the keys it support is only the PE that have been reistered
 * to the propagation structure by the time this is instantiated.
 * WARNING: this is not efficient if you do not actually use many of the keys
 * because the instantiated array will be very large compared to your benefits.
 * This might kill cache and RAM for nothing
 *
 * @param MaxNodeID
 * @tparam T
 */
class NodeDictionary[T](val MaxNodeID:Int)(implicit val X:Manifest[T]){
  private val storage:Array[T] = new Array[T](MaxNodeID-1)

  def update(elem:PropagationElement,value:T){
    storage(elem.UniqueID)=value
  }

  def get(elem:PropagationElement):T = storage(elem.UniqueID)
  
  def initialize(value:T){for (i <- storage.indices) storage(i) = value}
}

class StronglyConnectedComponent(val Elements: Iterable[PropagationElement],
                              val core: PropagationStructure, val _UniqueID: Int) extends PropagationElement with DAG {

  var ScheduledElements: List[PropagationElement] = List.empty

  UniqueID = _UniqueID

  for (e <- Elements) {e.component = this}

  def size: Int = Elements.size

  override def getPropagationStructure: PropagationStructure = core

  //for the DAG
  override def nodes = Elements.asInstanceOf[Iterable[DAGNode]]

  override def determineBoundary() {
    for (e <- Elements) {
      e.determineBoundary()
    }
    for (e <- Elements) {
      e.InitiateDynamicGraphFromSameComponent()
    }
  }

  def scheduleForPropagation(element: PropagationElement) {
    ScheduledElements = element :: ScheduledElements
    super.scheduleForPropagation()
  }

  var Stalls = 0
  def getStalls = Stalls

  def addDependency(from:PropagationElement, to:PropagationElement){
    try{
      notifyAddEdge(from,to)
    }catch{
      case c:CycleException => {
        //This can happen if we perform heavy changes to the dependencies in a careless way,
        // eg: reloading a previous model.
        // We wait for the dependencies to be stable, when the propagation is performed.

        autoSort = false
        Stalls +=1
      }
    }
  }

  val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.Position, size)

  override def performPropagation() {
    //setting autosort to true will not perform any operation unless it was set to false. This happens in two cases:
    //at the initial propagation, and when a stall just occurred. In these case, a non-incremental sort takes place
    autoSort = true

    for (e <- ScheduledElements) {
      h.insert(e)
    }
    ScheduledElements = List.empty

    var maxposition:Int = -1;
    
    while (!h.isEmpty) {
      val x = h.popFirst()
      x.propagate()
      assert(x.Position <= maxposition,"non monotonic propagation detected in SCC")
      assert({maxposition = x.Position; true})

      for (e <- ScheduledElements) {
        h.insert(e)
      }
      ScheduledElements = List.empty
    }
  }

  override def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (element <- Elements){
      toreturn = element.decrementSucceedingAndAccumulateFront(toreturn)
    }
    toreturn
  }

  override def setCounterToPrecedingCount(): Boolean = {
    Position = Elements.count(p => p.setCounterToPrecedingCount())
    (Position != 0)
  }

  def getDotNode: String = {
    throw new Exception("StrognlyConnectedComponent are handled as subgraph in dot files")
    ""
  }

  override private[core] def rescheduleIfNeeded() {}
  //we do nothing, since it is the propagation elements that trigger the registration if needed of SCC

  override def checkInternals(c:checker){
    for(e <-Elements){e.checkInternals(c)}
  }
}

object PropagationElement {
  implicit val Ord: Ordering[PropagationElement] = new Ordering[PropagationElement] {
    def compare(o1: PropagationElement, o2: PropagationElement) = o1.compareTo(o2) //the one of dagnode
  }
}

/**This class is used in Asteroid as a handle to register and unregister dynamically to variables*/
case class KeyForElementRemoval(element: PropagationElement
                                , KeyForListenedElement: PFDLLStorageElement[(PropagationElement, Any)]
                                , KeyForListeningElement: PFDLLStorageElement[PropagationElement])

/**this is a propagation element. It mainly defines:
 * it dependencies (static and dynamic), which are notably forwarded to the API of the DAGNode
 * its performPropagation and checkInternals methods
 * it also has a scheduleForPropagation method that can be invoked by custom code
 * to notify that this propagation element should be included in the coming or current propagation wave.
 *
 * There are two graph mentioning the dependencies of propagation elements:
 - a static propagation graph that does not change after the call to setupPropagationStructure
 - a dynamic graph whose edge can change dynamically, but are all included in the static propagation graph
 */
trait PropagationElement extends DAGNode with TarjanNode with DistributedStorageUtility{

  final def compare(that: DAGNode): Int = {
    assert(this.UniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    assert(that.UniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    this.UniqueID - that.UniqueID
  }

  /**this refers to the propagationComponent that contains the PropagationElement.
   * it is managed by the propagation structure
   */
  var component: StronglyConnectedComponent = null

  /**set to true if the PropagationElement is scheduled for propagation, false otherwise.
   * this is managed by the PropagationElement
   */
  var isScheduled: Boolean = false

  /**set to true if the PropagationElement is one that can break
   * or make dependency cycles in the dynamic dependency graph
   * managed by the PropagationComponent
   * basically, set to true if the determiningElement is not in the same component
   * and if this PropagationElement belongs to a cycle in the static dependency graph*/
  var IsBoundary: Boolean = false

  /**this sets the value of IsBoundary according to the definition of this variable
   * @return the value of IsBoundary*/
  def determineBoundary() {
    if (component == null) {
      IsBoundary = false
    } else {
      val a = getDeterminingElements
      if (a == null) {
        IsBoundary = false
      } else {
        IsBoundary = a.forall(p => (p.component != this.component))
      }
    }
  }

  var StaticallyListenedElements: List[PropagationElement] = List.empty

  var StaticallyListeningElements: List[PropagationElement] = List.empty

  var DeterminingElements: List[PropagationElement] = List.empty

  val DynamicallyListenedElements: PermaFilteredDoublyLinkedList[PropagationElement]
    = new PermaFilteredDoublyLinkedList[PropagationElement]

  val DynamicallyListeningElements: PermaFilteredDoublyLinkedList[(PropagationElement, Any)]
    = new PermaFilteredDoublyLinkedList[(PropagationElement, Any)]

  var DynamicallyListenedElementsFromSameComponent: PermaFilteredDoublyLinkedList[PropagationElement] = null

  var DynamicallyListeningElementsFromSameComponent: PermaFilteredDoublyLinkedList[PropagationElement] = null

  def InitiateDynamicGraphFromSameComponent() {
    assert(component != null)
    DynamicallyListenedElementsFromSameComponent
       = DynamicallyListenedElements.PermaFilter((e: PropagationElement) => e.component == component)
    DynamicallyListeningElementsFromSameComponent
      = DynamicallyListeningElements.PermaFilter((e) => e._1.component == component, (e) => e._1)
  }

  /**through this method, the PropagationElement must declare which PropagationElement it is listening to
   * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
   * to override*/
  final def getStaticallyListenedElements: Iterable[PropagationElement] = StaticallyListenedElements

  /**through this method, the PropagationElement must declare which PropagationElement listen to it
   * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
   * to override*/
  final def getStaticallyListeningElements: Iterable[PropagationElement] = StaticallyListeningElements

  final def getDynamicallyListeningElements: Iterable[(PropagationElement, Any)] = DynamicallyListeningElements

  final def getDynamicallyListenedElements: Iterable[PropagationElement] = DynamicallyListenedElements

  /**registers an element in the static dependency graph.
   * Beware that you also need to register elements in the dynamic propagation graph for something to happen.
   * @param p the element that we register to
   **/
  protected final def registerStaticallyListenedElement(p: PropagationElement) {
    StaticallyListenedElements = p :: StaticallyListenedElements
    p.StaticallyListeningElements = this :: p.StaticallyListeningElements
  }

  /**must belong to the statically listened elements.
   * cannot be added to the dynamically listened ones (it is added through this method, so you cannot remove it)
   * @param p the element that determines the dynamic dependencies of the propagation element
   * @param i an additional value that is stored in this element together with the reference to this,
   * can be use for notification purposes
   */
  protected final def registerDeterminingElement(p: PropagationElement, i: Any) {
    assert(this.getStaticallyListenedElements.exists(e => e == p),
      "dependency to determining element " + p + " must be registered in static propagation graph")
    registerDynamicallyListenedElement(p, i)
    DeterminingElements = p :: DeterminingElements
  }

  /**the variable that influence on the dependencies of the propagation element
   * these are propagated first to constitute the dynamic propagation graph*/
  def getDeterminingElements: Iterable[PropagationElement] = DeterminingElements

  /**this registers to the element in the dynamic propagation graph.
   * this element must have been registered o the static propagation graph before, or be accessible through a bulk
   * @param p the element that we register to
   * @param i a value that can be exploited by the element to notify its updates. normally, this value should be an int,
   *            if other type is used, the invariant should override a dedicated notification method.
   * @return a key that is needed to unregister the element in the dynamic propagation graph
   */
  protected def registerDynamicallyListenedElement(p: PropagationElement, i: Any): KeyForElementRemoval = {
    assert(isStaticPropagationGraphOrBulked(p, 1),
      "dependency to element " + p + " must be registered in static propagation graph before dynamic one")
    val KeyForListeningElement = DynamicallyListenedElements.addElem(p)
    val KeyForListenedElement = p.DynamicallyListeningElements.addElem((this, i))

    //We need to check for boundary here Because other elemenbts might indeed change their dependencies,
    // but since they are not boundary, this would require resorting the SCC during the propagation
    if (IsBoundary && p.component == this.component) {
      //this is only called once the component is established, so no worries.
      component.addDependency(p, this)
    }

    KeyForElementRemoval(p, KeyForListenedElement, KeyForListeningElement)
  }

  /**checks that the propagation element is statically listened to, possibly
   * up to the transitivity depth mentioned in depth
   * Beware: this is really not efficient, so do not call unless in heavy debug mode
   * it is called by all register methods, by the way
   * @param p the propagation element to find
   * @param depth the maximal transitivity depth (basically to ensure that we do not follow bulk chains)
   * @return true if found, false otherwise
   */
  private def isStaticPropagationGraphOrBulked(p: PropagationElement, depth: Int = 0): Boolean = {
    if (getStaticallyListenedElements == null) return true //static graph has been dropped
    for (q <- getStaticallyListenedElements) {
      if (p == q) return true
      if (q.isInstanceOf[BulkPropagator] && depth > 0 && q.isStaticPropagationGraphOrBulked(p, depth - 1)) return true
    }
    false;
  }

  /**
   * unregisters an element in the dynamic propagation graph.
   * @param p the key that was given when the eement was registered in the dynamic propagation graph
   */
  protected def unregisterDynamicallyListenedElement(p: KeyForElementRemoval) {
    DynamicallyListenedElements.deleteElem(p.KeyForListeningElement)
    p.element.DynamicallyListeningElements.deleteElem(p.KeyForListenedElement)
  }

  def dropStaticGraph() {
    StaticallyListenedElements = null
    StaticallyListeningElements = null
  }

  final def getDAGPrecedingNodes: Iterable[DAGNode]
    = DynamicallyListenedElementsFromSameComponent

  final def getDAGSucceedingNodes: Iterable[DAGNode]
    = DynamicallyListeningElementsFromSameComponent

  def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (succeeding <- getStaticallyListeningElements){
      if (succeeding.component == null || succeeding.component != this.component) {
        //not in the same SCC as us
        toreturn = succeeding.decrementAndAccumulateFront(toreturn)
      }
    }
    toreturn
  }

  final def decrementAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    Position -= 1
    if (Position == 0) {
      //faut pusher qqchose
      if (component != null) {
        component.decrementAndAccumulateFront(acc)
      } else {
        this :: acc
      }
    } else {
      acc
    }
  }

  /**Sets the Position oto the number of element that need to be decremented, not belonging to same connex component
   * for connex component, set it to the number of element that are referenced from othercomponents
   * @return true if there is a dependency, false otherwise
   */
  def setCounterToPrecedingCount(): Boolean = {
    //le compteur est mis au nombre de noeud precedent qui ne sont pas dans la meme composante connexe
    if (this.component == null) {
      Position = this.getStaticallyListenedElements.count(p => p.getPropagationStructure != null)
    } else {
      //it is in a SCC.
      Position = this.getStaticallyListenedElements.count(p => (p.component != this.component && p.getPropagationStructure != null))
    }
    Position != 0
  }

  /**returns a reference to the propagationStructure where the PropagationElement is registered*/
  def getPropagationStructure: PropagationStructure

  /**to invoque to force inclusion of the propagation element in the current or next propagation wave. */
  final def scheduleForPropagation() {
    assert(getPropagationStructure != null, "cannot schedule or propagate element out of propagation structure")
    if (!isScheduled) {
      if (this.getPropagationStructure.Verbose) println("PropagationStruture: scheduled [" + this + "]")
      isScheduled = true
      if (component == null) {
        getPropagationStructure.scheduleForPropagation(this)
      } else {
        component.scheduleForPropagation(this)
      }
    }
  }

  private[core] def rescheduleIfNeeded() {
    if (isScheduled) {
      if (this.getPropagationStructure.Verbose) println("PropagationStruture: re-scheduled [" + this.getClass + "]")
      if (component == null) {
        getPropagationStructure.scheduleForPropagation(this)
      } else {
        component.scheduleForPropagation(this)
      }
    }
  }

  /**Performs the propagation, and some bookkeeping around it.
   */
  final def propagate() {
    assert(isScheduled)
    assert(getPropagationStructure != null, "cannot schedule or propagate element out of propagation structure")
    assert({getPropagationStructure.PropagatingElement = this; true})
    if (getPropagationStructure.Verbose) println("PropagationStruture: propagating [" + this + "]")
    performPropagation()
    isScheduled = false //to avoid registering SCC to the propagation structure every time...
    assert({getPropagationStructure.PropagatingElement = null; true})
  }

  /**this is the propagation method that should be overridden by propagation elements.
   * notice that it is only called in a propagation wave if:
   * 1: it has been registered for propagation since the last time it was propagated
   * 2: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
   *    it only propagates the ones that come in the predecessors of the targeted propagation element
   *  overriding this method is optional, so an empty body is provided by default*/
  def performPropagation() {
    ;
  }

  /**This is the debug procedure through which propagation element can redundantly check
   * that the incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  def checkInternals(c:checker) {
    ;
  }

  /**This returns the dot node to display on the DOT output for the node. Only the argument of the nodes
   * example: "[label= \"toto\" shape=diamond color=red]"
   * */
  def getDotNode: String

}

/**This is the node type to be used for bulking
 **/
trait BulkPropagator extends PropagationElement

abstract trait checker{
  def check(b:Boolean)
}