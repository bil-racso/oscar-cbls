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


package oscar.cbls.invariants.core.algo.dag

trait DAGNode extends Ordered[DAGNode]{

  /**the position in the topological sort*/
  var Position: Int = 0;

  /**supposed to be false between each pass of the algorithm*/
  var visited: Boolean = false;
  var visited2:Boolean = false;

  /**it gives the unique ID of the PropagationElement.
   * those uniqueID are expected to start at 0 and to increase continuously
   * An exception is tolerated: UniqueID is set to -1
   * if the Propagation Element is not mentioned in the propagation structure, such as for constants
   * yet is mentioned in the dependencies of registered propagation elements
   */
  var UniqueID:Int = -1

  def getDAGPrecedingNodes: Iterable[DAGNode]
  
  def getDAGSucceedingNodes: Iterable[DAGNode]
}

class CycleException(n: DAGNode) extends Exception

/**This data structure performs dynamic topological sort on DAG
 * the topological sort can be performed either from scratch or maintained incrementally.
 * The topological sort is about maintaining the attribute Position in the nodes [[oscar.cbls.invariants.core.algo.dag.DAGNode]]
 *
 * the topological sort is lower before
 *
 * The incremental topological sort in setAutoSort(mAutoSort: Boolean){
 */
trait DAG {
  private var AutoSort: Boolean = false;
  def getNodes:Iterable[DAGNode]


  /**performs a self-check on the ordering, use for testing*/
  def checkSort(){
    for (to <- getNodes){
      for(from <- to.getDAGPrecedingNodes){
        assert(from.Position < to.Position,"topological sort is wrong at " + from + "->" + to)
      }
    }
  }

  /**Checks that node have correct reference to each othe. Nodes are expected to know their successors and predecessors.
   * This is expected to be consistent between several nodes.
   */
  def checkGraph(){
     getNodes.foreach(n => {
      n.getDAGPrecedingNodes.foreach(p=> {
        if(!p.getDAGSucceedingNodes.exists(p => p == n)){
            throw new Exception("graph is incoherent at nodes [" + p + "] -> [" + n +"]")
        }
      })

      n.getDAGSucceedingNodes.foreach(p=> {
        if(!p.getDAGPrecedingNodes.exists(p => p == n)){
            throw new Exception("graph is incoherent at nodes [" + n + "] -> [" + p +"]")
        }
      })
     })
  }

  /**turns the incremental sort on or off.
   * Incremental sort is then applied at each edge insert. node insert and delete is prohibited when autosort is activated
   * in case a cycle is detected, does not pass in autosort model, but throws an exception  
   */
  def setAutoSort(mAutoSort: Boolean){
    if (mAutoSort && !AutoSort) {
      //on passe en eautosort
      doDAGSort();
      assert({checkSort(); checkGraph(); true})
      //will throw an exception in case of cycle, so AutoSort will not be set to true
      AutoSort = true;
    } else if (AutoSort && ! mAutoSort) {
      //on sort de l'autosort
      AutoSort = false;
    }
  }

  /**@return the autosort status*/
  def getAutoSort:Boolean = AutoSort

  /**to notify that an edge has been added between two nodes.
   * this will trigger a re-ordering of the nodes in the topological sort if it is activated.
   * The reordering might lead to an exception [[oscar.cbls.invariants.core.algo.dag.CycleException]] in case there is a cycle in the graph
   * We expect the graph to be updated prior to calling this method
   * notice that you do not need to notify edge deletion.
   */
  def notifyAddEdge(from: DAGNode, to: DAGNode) {

    if (AutoSort && (from.Position > to.Position)) {
      //refaire le sort
      //discovery
      val SortedForwardRegion = findForwardRegion(to, from.Position).sortWith((p, q) => p.Position < q.Position)

      val SortedBackwardsRegion = findBackwardsRegion(from, to.Position).sortWith((p, q) => p.Position < q.Position)
      //reassignment

      val FreePositionsToDistribute: List[Int] = mergeNodeLists(SortedForwardRegion, SortedBackwardsRegion)

      val FreePositionsForForwardRegion = realloc(SortedBackwardsRegion, FreePositionsToDistribute)
      realloc(SortedForwardRegion, FreePositionsForForwardRegion )

      assert({checkSort(); checkGraph(); true})
    }
  }

  //retourne un cycle, pour aider au debugging
  //pre: il y a un cycle dans le Algo
  //argument optionel: un noeud implique dans le cycle: on commence par chercher un cycle impliquant ce noeud.
  //si pas de cycle, retourne null.
  def getCycle(Start:DAGNode=null):List[DAGNode] = {

    //on marque visite quand on poppe de la DFS ou quand on est retombe sur le debut du cycle
    var ExploredStack:List[DAGNode] = List.empty //upside down

    def DFS(n:DAGNode):Boolean = { //return true si on a trouve un cycle
      if(n.visited) return false
      if(n.visited2){  //found a cycle
        ExploredStack = (n :: ExploredStack).reverse
        n.visited=true
        while(!ExploredStack.head.visited){ExploredStack = ExploredStack.tail}
        getNodes.foreach(p => {p.visited = false; p.visited2 = false})
        true
      }else{ //not yet
        n.visited2 = true
        ExploredStack = n :: ExploredStack
        n.getDAGSucceedingNodes.foreach(p => {if(DFS(p)){return true}})
        n.visited=true
        n.visited2 = false
        ExploredStack = ExploredStack.tail
        false
      }
    }

    if(Start != null){
      if(DFS(Start)){ return ExploredStack }
    }
    getNodes.foreach(n => {
      if (!n.visited)
        if (DFS(n)){return ExploredStack}
    })
    getNodes.foreach(p => {p.visited = false; p.visited2 = false})
    null
  }

  /**sorts DAG nodes according to dependencies.
   * first position is set to zero.
   * this throws an exception [[oscar.cbls.invariants.core.algo.dag.CycleException]] in case a cycle is detected
   */
  def doDAGSort() {
    //on utilise les positions pour stocker le nombre de noeuds predecesseurs non visites, puis on met l'autre valeur apres.
    getNodes.foreach(n => n.Position = n.getDAGPrecedingNodes.size)
    var Front: List[DAGNode] = getNodes.toList.filter(n => (n.Position == 0))
    var Position = 0 //la position du prochain noeud place.
    while (!Front.isEmpty) {
      val n = Front.head
      Front = Front.tail
      n.Position = Position
      Position += 1
      n.getDAGSucceedingNodes.foreach(p => {
        p.Position -=1
        if (p.Position == 0) Front = (p::Front) //une stack, en fait, mais c'est insensitif, puis c'est plus rapide. 
      })
    }
    if (Position != getNodes.size) {
      throw new CycleException(null)
    }
  }

  private def findForwardRegion(n: DAGNode, ub: Int): List[DAGNode] = {
    def dfsF(n: DAGNode, acc: List[DAGNode]): List[DAGNode] = {
      n.visited = true
      var newlist = n :: acc
      //est-ce bien correct de faire une DFS?
      n.getDAGSucceedingNodes.foreach(p => {
        if (p.Position == ub) {
          getNodes.foreach(q => q.visited = false)
          throw new CycleException(p)
        }
        if (!p.visited && p.Position < ub) {
          newlist = dfsF(p, newlist)
        }
      })
      newlist
    }
    dfsF(n, List.empty)
  }

  private def findBackwardsRegion(n: DAGNode, lb: Int): List[DAGNode] = {
    def dfsB(n: DAGNode, acc: List[DAGNode]): List[DAGNode] = {
      n.visited = true
      var newlist = n :: acc
      n.getDAGPrecedingNodes.foreach(p => {
        if (!p.visited && p.Position > lb) {
          newlist = dfsB(p, newlist)
        }
      })
      newlist
    }
    dfsB(n, List.empty)
  }

  //merge deux listes de noeuds triee par position, donne la position triee de ces noeuds
  private def mergeNodeLists(a: List[DAGNode], b: List[DAGNode]): List[Int] = {
    if (a.isEmpty && b.isEmpty){
      List.empty[Int]
    }else if (a.isEmpty) {
      b.head.Position :: mergeNodeLists(a, b.tail)
    } else if (b.isEmpty) {
      a.head.Position :: mergeNodeLists(a.tail, b)
    } else if (a.head.Position < b.head.Position) {
      a.head.Position :: mergeNodeLists(a.tail, b)
    } else {
      b.head.Position :: mergeNodeLists(a, b.tail)
    }
  }

  private def realloc(OrderedNodeForReinsertion: List[DAGNode], FreePositionsToDistribute: List[Int]):List[Int] = {
    if (!OrderedNodeForReinsertion.isEmpty) {
      OrderedNodeForReinsertion.head.visited = false
      OrderedNodeForReinsertion.head.Position = FreePositionsToDistribute.head
      realloc(OrderedNodeForReinsertion.tail, FreePositionsToDistribute.tail)
    }else{
      FreePositionsToDistribute
    }
  }
}
