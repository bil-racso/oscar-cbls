package oscar.cp.constraints.sat

import collection.immutable.SortedSet
import collection.immutable.SortedMap

// Decomposes the graph given by successors in SCCs, and stores the decomposition
class Tarjan(ids: Array[Int], successors: Array[Array[Int]]) {
  val succs = getStronglyConnectedComponents(ids, n => successors(n))
  
  // takes an sccId, yields the nodes of sccId
  val nodesOf = succs.map(_.toArray).toArray
  
  // takes a node, yields its sccId
  val sccOf = Array.tabulate(ids.size) { x => 0 }
  nodesOf.zipWithIndex.foreach { case (scc, sccI) =>
    scc.foreach { literal =>
      sccOf(literal) = sccI
    }
  }
  
  // takes an sccId, yields the sccIds of its successors
  val successorsOf = nodesOf.zipWithIndex.map { case (nodes, sccI) => nodes.map {
   node => successors(node).map(succ => sccOf(succ))
  }.flatten.filter(_ != sccI).distinct }


  def getStronglyConnectedComponents(Nodes:Iterable[Int], GetSucceedingNodes:(Int => Iterable[Int])):List[SortedSet[Int]] = {
    // code stolen from oscar/cbls
    var Index: SortedMap[Int,Int] = SortedMap.empty
    var LowLink: SortedMap[Int,Int] = SortedMap.empty
    var index:Int=0
    var Stack:List[Int]=List.empty
    var StackSet:SortedSet[Int] = SortedSet.empty
    var Components:List[SortedSet[Int]]= List.empty
    var InOneComponent:SortedSet[Int] = SortedSet.empty

    def visit(v:Int){
      Index += ((v,index))
      LowLink+=((v,index))
      index +=1
      Stack = v::Stack
      StackSet +=v
      // Consider successors of v
      for(w <- GetSucceedingNodes(v)){
        if(!Index.contains(w)){
          // Successor w has not yet been visited; recurse on it
          visit(w)
          LowLink+=(( v, LowLink(v).min(LowLink(w)) ))
        }else if(StackSet.contains(w)){
          // Successor w is in stack S and hence in the current SCC
          LowLink+=(( v,LowLink(v).min(Index(w)) ))
        }
      }

      // If v is a root node, pop the stack and generate an SCC
      if (LowLink(v) == Index(v)){
        //start a new strongly connected component
        var SCC:SortedSet[Int] = SortedSet.empty[Int]
        var finished:Boolean = false
        while(!finished){
          val node = Stack.head
          Stack = Stack.tail
          StackSet -= node
          SCC +=node
          InOneComponent += node
          finished = (node == v)
        }
        Components = SCC :: Components
      }
    }

    for(n <- Nodes) {if(!Index.contains(n)) visit(n)}

//    for(n <- Nodes){if (!InOneComponent.contains(n)){
//      Components = SortedSet(n) :: Components
//    }}

    Components
  }
}


object TestTarjan extends App {
  val G = Array(Array(1), Array(2), Array(1, 3), Array[Int]())
  val D = new Tarjan(Array(0, 1, 2, 3), G)
  println("nodesOf\n"      + D.nodesOf.map(_.mkString("[", ", ", "]")).mkString("[\n ", ",\n ", "\n]"))
  println("sccOf\n"        + D.sccOf.mkString("[", ", ", "]"))
  println("successorsOf\n" + D.successorsOf.map(_.mkString("[", ", ", "]")).mkString("[\n ", ",\n ", "\n]"))
}
