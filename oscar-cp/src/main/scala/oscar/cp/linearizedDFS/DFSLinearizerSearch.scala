package oscar.cp.linearizedDFS

import java.io.{FileWriter, PrintWriter}
import oscar.algo.search._
import oscar.cp.core.CPSolver
import oscar.algo.array.ArrayStack

/**
 * Created by saschavancauwelaert on 28/11/14.
 */
class DFSLinearizerSearch(node: CPSolver) {

  var searchStateModificationList = List[Decision]()

  private val alternativesStack = new ArrayStack[Iterator[Alternative]](100)

  // Number of backtracks of the previous search
  private var nbBkts: Int = 0

  // Number of solutions of the previous search
  private var nbSols: Int = 0

  // Number of nodes explored in the previous search
  private var nbNodes: Int = 0

  // Actions to execute in case of solution node
  private var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private var failureActions = List.empty[() => Unit]

  /** Returns the number of backtracks in the previous search */
  final def nBacktracks: Int = nbBkts

  /** Returns the number of solutions found in the previous search */
  final def nSolutions: Int = nbSols

  /** Returns the number nodes explored in the previous search */
  final def nNodes: Int = nbNodes

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  @inline private def expand(branching: Branching): Boolean = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) false
    else {
      alternativesStack.push(alternatives.iterator)
      true
    }
  }

  final def start(branching: Branching, stopCondition: => Boolean, savingDirURL : Option[String], maxNumDecisionPerFile : Int): (Int,Int,Int) = start(branching, (s:DFSLinearizerSearch) => stopCondition, savingDirURL, maxNumDecisionPerFile)

  final def start(branching: Branching, stopCondition: DFSLinearizerSearch => Boolean = _ => false, savingDirURL : Option[String] = None, maxNumDecisionPerFile : Int = Int.MaxValue) : (Int,Int,Int) = {

    var decisionChunkNumber = 0

    def saveCurrentDecisionsChunk(): Unit = {
      savingDirURL match {
        case None => //decision list will be kept in RAM
        case Some(url) => {
          val out = new PrintWriter(new FileWriter(url + "chunk" + decisionChunkNumber + ".txt", true))
          for (d <- searchStateModificationList.reverse)
            out.println(d)
          out.close()
          decisionChunkNumber += 1
          searchStateModificationList = Nil
        }
      }
    }

    // Initializes the search
    node.resetStats() // resets trailing time too
    alternativesStack.clear()
    nbSols = 0
    nbBkts = 0
    nbNodes = 0

    searchStateModificationList = Push(node) :: searchStateModificationList
    searchStateModificationList.head() //node.pushState()

    // Expand the root node
    if (!node.isFailed) {
      searchStateModificationList = Push(node) :: searchStateModificationList
      searchStateModificationList.head() //node.pushState()
      val isExpandable = expand(branching)
      if (!isExpandable) {
        node.solFound()
        solutionActions.foreach(_())
        nbSols += 1
      }
    }

    while (!alternativesStack.isEmpty && !stopCondition(this)) {

      //if the decision list too big, we save it inside a file
      if(searchStateModificationList.length >= maxNumDecisionPerFile)
        saveCurrentDecisionsChunk()

      nbNodes += 1

      val alternatives = alternativesStack.top
      val alternative = alternatives.next()

      val isLast = !alternatives.hasNext

      if (!isLast) {
        searchStateModificationList = Push(node) :: searchStateModificationList
        searchStateModificationList.head() //node.pushState()
      }
      else alternativesStack.pop() // no more alternative in the sequence

      if(alternative.isInstanceOf[Decision]) {
        searchStateModificationList = alternative.asInstanceOf[Decision] :: searchStateModificationList
      }

      alternative() // apply the alternative

      if (!node.isFailed()) {
        val isExpandable = expand(branching)
        if (!isExpandable) {
          node.solFound()
          solutionActions.foreach(_())
          nbSols += 1
          nbBkts += 1
          searchStateModificationList = Pop(node) :: searchStateModificationList
          searchStateModificationList.head() //node.pop()
        }
      } else {
        failureActions.foreach(_())
        nbBkts += 1
        searchStateModificationList = Pop(node) :: searchStateModificationList
        searchStateModificationList.head() //node.pop()
      }
    }

    // Pop the remaining nodes
    var i = alternativesStack.size
    while (i != 0) {
      searchStateModificationList = Pop(node) :: searchStateModificationList
      searchStateModificationList.head() //node.pop()
      i -= 1
    }
    searchStateModificationList = Pop(node) :: searchStateModificationList
    searchStateModificationList.head() //node.pop()

    //save the remaining decisions chunk
    saveCurrentDecisionsChunk()

    (nbSols,nbBkts,nbNodes)
  }

}
