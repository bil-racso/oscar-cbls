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
 *         by Renaud De Landtsheer and Christophe Ponsard
 ******************************************************************************/

package oscar.examples.cbls

import scala.swing.Applet
import scala.swing.Button
import scala.swing.event.ButtonClicked
import scala.swing.BoxPanel
import scala.swing.Orientation
import javax.swing.ImageIcon
import scala.swing.Label
import scala.swing.GridPanel
import java.awt.Color
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.modeling.Algebra._
import javax.swing.border.LineBorder

class NQueensApplet extends Applet {

  val NMIN: Int = 4
  val NMAX: Int = 30
  var N: Int = 15
  var Dim = Range(0, N)
  var PAUSE: Int = 50 + 1000 / N
  // UI stuff
  val cl = Thread.currentThread().getContextClassLoader()
  val QUEEN = new ImageIcon(   cl.getResource("src/main/scala/oscar/cbls/constraints/tests/resources/queen-ok.png"))
  val CONFLICT = new ImageIcon(cl.getResource("src/main/scala/oscar/cbls/constraints/tests/resources/queen-ko.png"))
  val EMPTY = new ImageIcon(   cl.getResource("src/main/scala/oscar/cbls/constraints/tests/resources/queen-no.png"))

  var gridPanel: GridPanel = null
  var tab: Array[Array[Label]] = null
  var lNQueen: Label = null

  // control stuff
  var stopRequested = false
  var solverRunning = false

  object ui extends UI with SearchEngineTrait {
    def init() = {

      contents = new BoxPanel(Orientation.Vertical) {
        gridPanel = new GridPanel(N, N) {
          background = Color.WHITE
        }
        contents += gridPanel

        contents += new BoxPanel(Orientation.Horizontal) {
          val bLess = new Button() {
            text = "Less (min "+NMIN+")"
          }
          lNQueen = new Label("  " + N + "  ")
          val bMore = new Button() {
            text = "More (max "+NMAX+")"
          }
          val lSpace = new Label("      ")
          val bRestart = new Button() {
            text = "Restart"
          }
          contents.append(bLess, lNQueen, bMore, lSpace, bRestart)
          listenTo(bLess, bMore, bRestart)
          reactions += {
            case ButtonClicked(`bLess`) => if (N > NMIN) resetProblem(N - 1)
            case ButtonClicked(`bMore`) => if (N < NMAX) resetProblem(N + 1)
            case ButtonClicked(`bRestart`) => resetProblem(N)
          }
        }
      }
      reInitGrid()
      
      startSolverInThread()
    }

    def startSolverInThread() {
      new Thread {
        override def run() {
          stopRequested = false
          solverRunning = true
          solve()
          solverRunning = false
        }
      }.start()
    }

    def resetProblem(n: Int) {
      if (stopRequested) return // not reentering

      new Thread {
        override def run() {
          stopRequested = true
          while (solverRunning == true) {
            Thread.sleep(50)
          }
          N=n
          reInitGrid()
          gridPanel.revalidate()
          repaint()
          lNQueen.text = "  " + N + "  "
          PAUSE = 50 + 1000 / N;
          startSolverInThread()
        }
      }.start()
    }

    def reInitGrid() {
      Dim = Range(0, N)
      tab = Array.ofDim[Label](N, N)
      
      gridPanel.rows=N
      gridPanel.columns=N
      gridPanel.contents.clear()

      val LineBorder=new LineBorder(Color.BLACK,1)
      
      for (i <- Dim; j <- Dim) {
        tab(i)(j) = new Label("")
        tab(i)(j).icon = EMPTY
        tab(i)(j).border = LineBorder
        gridPanel.contents += tab(i)(j)
      }
    }

    // this is duplicated code from demo purposes
    def solve() {
      val min = 0
      val max = N - 1
      val range: Range = Range(0, N)
      val tabulength = 0
      val m: Model = new Model(false, None, true)
      val MaxIT = 10000

      println("NQueens(" + N + ")")
      val Queens: Array[IntVar] = new Array[IntVar](N)
      for (q <- range) {
        Queens(q) = IntVar(m, min, max, q, "queen" + q)
        tab(q)(q).icon = CONFLICT
      }

      val c: ConstraintSystem = new ConstraintSystem(m)
      //c.post(AllDiff(Queens)) handled trough permutations
      c.post(AllDiff(for (q <- range) yield (q + Queens(q)).toIntVar))
      c.post(AllDiff(for (q <- range) yield (q - Queens(q)).toIntVar))

      val viol: Array[IntVar] = (for (q <- range) yield c.violation(Queens(q))).toArray
      c.close()
      m.close()

      var it: Int = 0
      val Tabu = (for (q <- range) yield -1).toArray

      var longueurplateau = 0;
      while ((c.Violation.value > 0) && (it < MaxIT) && !stopRequested) {
        val oldviolation: Int = c.Violation.value
        val allowedqueens = range.filter(q => Tabu(q) < it)
        val (q1, q2) = selectMin(allowedqueens, allowedqueens)((q1,q2) => c.swapVal(Queens(q1), Queens(q2)), (q1,q2) => q1 < q2)

        swapQueens(q1, q2)
        Tabu(q1) = it + tabulength
        Tabu(q2) = it + tabulength

        it += 1
        println("it: " + it + " " + c.Violation + " (swapped " + q1 + " and " + q2 + ")")
        if (oldviolation <= c.Violation.value) longueurplateau += 1 else longueurplateau = 0

        if (longueurplateau > 5) {
          println("jump away")
          for (i <- 1 to N / 5) {
            swapQueens(selectFrom(range), selectFrom(range))
          }
          longueurplateau = 0
        }

        Thread.sleep(PAUSE)
      }
      println("SOLVER ENDED")

      // subprocedure with also update the UI
      // TODO use Event to separate model from view and update more efficiently
      def swapQueens(q1: Int, q2: Int) {
        tab(q1)(Queens(q1).value).icon = EMPTY
        tab(q2)(Queens(q2).value).icon = EMPTY
        Queens(q1) :=: Queens(q2)
        if (viol(q1).value > 0)
          tab(q1)(Queens(q1).value).icon = CONFLICT
        else
          tab(q1)(Queens(q1).value).icon = QUEEN
        if (viol(q2).value > 0)
          tab(q2)(Queens(q2).value).icon = CONFLICT
        else
          tab(q2)(Queens(q2).value).icon = QUEEN

        // updating possible violations
        for (q <- range) {
          if (viol(Queens(q).value).value > 0) {
            tab(q)(Queens(q).value).icon = CONFLICT
          } else {
            tab(q)(Queens(q).value).icon = QUEEN
          }
        }
      }
    }
  }

}
