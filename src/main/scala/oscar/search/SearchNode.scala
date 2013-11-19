/*******************************************************************************
" * OscaR is free software: you can redistribute it and/or modify
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


package oscar.search

import java.util.Random
import java.util.Stack
import scala.util.continuations._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import oscar.reversible.ReversibleBool
import oscar.reversible.Trail
import oscar.reversible.TrailEntry
import oscar.reversible.ReversibleNode

/**
 * Class representing a (reversible search) node <br>
 * A search node is used to find solution by exploration of search tree (see Search).
 * @author Pierre Schaus pschaus@gmail.com
 */
class SearchNode extends ReversibleNode {

  var silent = false


  val random: Random = new Random(0)
  val failed = new ReversibleBool(this, false)

  var sc: SearchController = new DFSSearchController(this)
  
  

  /**
   * time (ms) spend in last exploration
   */
  var time: Long = 0
  /**
   * number of backtracks in last exploration
   */
  var bkts: Int = 0

  private var fLimit = Int.MaxValue
  private var tLimit = Int.MaxValue

  // tree visu
  val tree = new Tree(false)
  var currParent = 0
  var nodeMagic = 0
  def recordTree() {
    tree.clear()
    tree.record = true
  }

  /**
   *
   * @return The Random generator of this node potentially used in other algorithms
   */
  def getRandom() = random

  /**
   *
   * @return  true if this node can surely not lead to any solution
   */
  def isFailed(): Boolean = failed.value

  /**
   * Set the node in a failed state
   */
  def fail() {
    failed.setValue(true)
  }

  /**
   * @return the number of fail
   */
  def nFail() = sc.nFail()

  /**
   * Exit the search in progress and/or the LNS if any
   */
  def stop() {
    sc.stop()
  }

  def solFound() = {
    tree.addSuccess(currParent)
  }


  override def toString() = {
    super.toString
  }

  /**
   * executed just before the actual branch action
   */
  def beforeBranch() = {}

  /**
   * executed just after the actual branch action
   */
  def afterBranch() = {}
  
  
  def branch(left: => Unit)(right: => Unit):  Unit@suspendable =  {
    branchLabel("")(left)("")(right)
  }
  
  def branchLabel(leftLabel: String)(left: => Unit)( rightLabel: String)(right: => Unit) = {
    val idleft = nodeMagic + 1
    val idright = nodeMagic + 2
    nodeMagic += 2
    val parent = currParent

    shift { k: (Unit => Unit) =>
      if (!isFailed) {
        pushState()
        sc.addChoice(new MyContinuation("right", {
          tree.addBranch(parent, idright, idright.toString, rightLabel)
          pop()
          sc.fail()
          if (!isFailed) {
            beforeBranch()
            currParent = idright
            right
            afterBranch()
          }
          if (!isFailed()) {
            k()
          }
        }))
        sc.addChoice(new MyContinuation("left", {
          tree.addBranch(parent, idleft, idleft.toString, leftLabel)
          if (!isFailed()) {
            beforeBranch()
            currParent = idleft
            left
            afterBranch()
          }
          if (!isFailed()) {
            k()
          }
        }))
      }
    }
  }
  
  def branchAllLabel[A](indexes: Seq[A])(l: A => String)(f: A => Unit) = {
    val idleft = nodeMagic + 1
    val idright = nodeMagic + 2
    val idchildren = Array.tabulate(indexes.length)(nodeMagic+_+1)
    val parent = currParent    
    nodeMagic += indexes.length

    shift { k: (Unit => Unit) =>
      val first = indexes.head
      for ((i,j) <- indexes.reverse.zipWithIndex) {
        if (i != first) pushState()
        sc.addChoice(new MyContinuation("i", {
          tree.addBranch(parent, idchildren(j), idchildren(j).toString, l(i))
          if (i != first) {
            pop()
            sc.fail()
          }
          beforeBranch()
          currParent = idchildren(j)
          f(i)
          afterBranch()
          if (!isFailed()) k()
        }))

      }
    }
  }  

  def branchAll[A](indexes: Seq[A])(f: A => Unit) = {
    val idleft = nodeMagic + 1
    val idright = nodeMagic + 2
    val idchildren = Array.tabulate(indexes.length)(nodeMagic+_+1)
    val parent = currParent    
    nodeMagic += indexes.length

    shift { k: (Unit => Unit) =>
      val first = indexes.head
      for ((i,j) <- indexes.reverse.zipWithIndex) {
        if (i != first) pushState()
        sc.addChoice(new MyContinuation("i", {
          tree.addBranch(parent, idchildren(j), idchildren(j).toString, "")
          if (i != first) {
            pop()
            sc.fail()
          }
          beforeBranch()
          currParent = idchildren(j)
          f(i)
          afterBranch()
          if (!isFailed()) k()
        }))

      }
    }
  }

  def branchOne(left: => Unit): Unit @suspendable = {
    shift { k: (Unit => Unit) =>
      left
      if (!isFailed()) k()
    }
  }

  case class Exploration(val exploration: () => Unit @suspendable)
  private var exploBlock: Option[Exploration] = None

  def exploration(block: => Unit @suspendable): SearchNode = {
    exploBlock = Option(Exploration(() => block))
    this
  }

  var explorationCompleted = false

  /**
   * Start the exploration block
   * @param: nbSolMax is the maximum number of solution to discover before the exploration stops (default = Int.MaxValue)
   * @param: failureLimit is the maximum number of backtracks before the exploration stops (default = Int.MaxValue)
   * @param: timeLimit is the maximum number of milliseconds before the exploration stops (default = Int.MaxValue)
   */
  def run(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue) = runSubjectTo(nbSolMax, failureLimit, timeLimit)()

  protected def update() = {}

  /**
   * Start the exploration block
   * @param: nbSolMax is the maximum number of solution to discover before the exploration stops (default = Int.MaxValue)
   * @param: failureLimit is the maximum number of backtracks before the exploration stops (default = Int.MaxValue)
   * @param: timeLimit is the maximum number of milliseconds before the exploration stops (default = Int.MaxValue)
   * @param: reversibleBlock is bloc of code such that every constraints posted in it are removed after the run
   */
  def runSubjectTo(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue)(reversibleBlock: => Unit = {}): Unit = {
    val t1 = System.currentTimeMillis()
    explorationCompleted = false
    
    // Fix failure limit
    sc.failLimit_=(failureLimit)
    fLimit = failureLimit    
    // Fix time limit
    sc.timeLimit_=(timeLimit)
    tLimit = timeLimit
    var n = 0
    reset {
      shift { k1: (Unit => Unit) =>
        val b = () => {
          sc.start()
          update()
          if (!isFailed()) {
            exploBlock.get.exploration()
          } else {
            shift { k: (Unit => Unit) => k() }
          }
          if (!isFailed()) {
            n += 1 // one more sol found
            solFound()
            if (n == nbSolMax) {
              bkts = sc.nFail
              sc.reset() // stop the search
              k1() // exit the exploration block
            }
          }
        }
        popAll()
        pushState()
        reversibleBlock
        sc.reset()
        if (!isFailed()) {
          sc.reset()
          var exploreStarted = false
          reset {
            b()
            if (exploreStarted) {
              exploreStarted = true
            }
          }
          if (!sc.exit) sc.explore() // let's go, unless the user decided to stop
        }
        k1() // exit the exploration block       
      }
    }
    bkts = sc.nFail max bkts
    time = System.currentTimeMillis() - t1
    if (!sc.isLimitReached && n < nbSolMax) {
      explorationCompleted = true
    }
    if (explorationCompleted) {
      if (!silent) print("R")
    } else {
      if (!silent) print("!")
    }
  }
  private var branchings = new BranchingCombinator()
  
  def search(block: => Seq[Alternative]): SearchNode = {
    val b = new Branching() {
      override def alternatives = {
        block
      }
    }
    branchings.addBranching(b)
    this
  }
  
  def search(branching: Branching): SearchNode = {
    branchings.addBranching(branching)
    this
  }  
  
  private var solCallBacks = List[() => Unit]()
  
  def onSolution(block: => Unit): SearchNode = {
    solCallBacks = (() => block) :: solCallBacks
    this
  }
  
  def startSubjectTo (nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue)(reversibleBlock: => Unit = {}): List[(String,Int)] = {
    pushState()
    reversibleBlock
    val s = new Search(this,branchings)
    solCallBacks.foreach(b => s.onSolution(b()))
    s.onSolution(solFound())
    
    val t0 = System.currentTimeMillis()
    val stats =  s.solveAll(nbSol = nbSolMax, maxDiscrepancy = Int.MaxValue)
    stats ++ List(("time(ms)", (System.currentTimeMillis() - t0).toInt),
      ("time in trail restore(ms)", getTrail().getTimeInRestore().toInt),
      ("max trail size", getTrail().getMaxSize()))
    
    //List(("%% time in trail restore(ms) : ", getTrail().getTimeInRestore()))
    
    
    /*
    println("%% time(ms) : "+ time)
    println("%% #bkts : "+ bkts)
    println("%% time in fix point(ms) : "+ timeInFixPoint)
    println("%% time in trail restore(ms) : "+ getTrail().getTimeInRestore())
    println("%% max trail size : "+ getTrail().getMaxSize())
    */
  }
  
  def start(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue): List[(String,Int)] = {
    startSubjectTo(nbSolMax,failureLimit,timeLimit)()
  }

}
