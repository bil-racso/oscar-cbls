package oscar.lcg.constraints

import oscar.cp.core.CPStore
import oscar.lcg.core.CDCLStore
import oscar.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.lcg.core.Literal
import oscar.lcg.core.LCGSolver
import oscar.lcg.core.True
import oscar.algo.reversible.ReversibleInt

/** @author Renaud Hartert ren.hartert@gmail.com */
class DecompCumulative(starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int) extends LCGConstraint(starts(0).cpStore, "DecompTT") {

  require(starts.length > 0)

  private[this] val nTasks = starts.length

  private[this] val actives = Array.fill(horizon)(Array.tabulate(nTasks)(i => i))
  private[this] var nActives = nTasks
  private[this] val nActivesRev = Array.fill(horizon)(new ReversibleInt(s, nTasks))

  private[this] var gap = capa
  private[this] val gapRev = Array.fill(horizon)(new ReversibleInt(s, capa))

  private[this] val mandatories = Array.fill(horizon)(new Array[Int](nTasks))
  private[this] var nMandatories = 0
  private[this] val nMandatoriesRev = Array.fill(horizon)(new ReversibleInt(s, 0))

  private[this] val updateStarts = new Array[Int](nTasks)
  private[this] val updateEnds = new Array[Int](nTasks)
  private[this] var nUpdateStarts = 0
  private[this] var nUpdateEnds = 0
  
  private[this] val timePoints = new Array[Int](nTasks)

  private[this] val builder = cdclStore.clauseBuilder

  final override def cdclStore = starts(0).cdclStore

  private[this] val overlaps = Array.tabulate(horizon, nTasks)((time, task) => {
    val start = starts(task)
    // Literals
    val literal = cdclStore.newVariable(null, "[" + start.name + " overlaps " + time + "]", "[" + start.name + " not_overlaps " + time + "]")
    val lit1 = start.greaterEqual(time - durations(task) + 1)
    val lit2 = start.lowerEqual(time)
    // First clause: lit1 and lit2 => literal
    builder.clear()
    builder.add(-lit1)
    builder.add(-lit2)
    builder.add(literal)
    cdclStore.addProblemClause(builder.toArray)
    // Second clause: literal => lit1
    builder.clear()
    builder.add(-literal)
    builder.add(lit1)
    cdclStore.addProblemClause(builder.toArray)
    // Third clause: literal => lit2
    builder.clear()
    builder.add(-literal)
    builder.add(lit2)
    cdclStore.addProblemClause(builder.toArray)
    literal
  })

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) return Failure
    else {
      var i = 0
      while (i < nTasks) {
        starts(i).callWhenBoundsChange(this)
        i += 1
      }
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    
    var i = nTasks
    while (i > 0) {
      i -= 1
      timePoints(i) = starts(i).min + durations(i) - 1
    }

    var fail = false
    var time = horizon
    i = nTasks
    while (i > 0 && !fail) {
      i -= 1
      val time = timePoints(i)

      // Cache
      nActives = nActivesRev(time).value

      if (nActives > 0) {

        // Cache
        gap = gapRev(time).value
        nMandatories = nMandatoriesRev(time).value

        // Reset structure
        nUpdateStarts = 0
        nUpdateEnds = 0

        // Compute 
        var i = nActives
        while (i > 0 && gap >= 0) {
          i -= 1
          val task = actives(time)(i)
          val lit = overlaps(time)(task)
          val est = starts(task).min
          val ect = est + durations(task)
          val lst = starts(task).max
          val lct = lst + durations(task)
          if (lct <= time) deactivate(time, i)
          else if (time < est) deactivate(time, i)
          else if (lst <= time && time < ect) {
            // The task is mandatory
            mandatories(time)(nMandatories) = task
            nMandatories += 1
            // The task is not active
            deactivate(time, i)
            // Update gap
            gap -= demands(task)
          } else if (est <= time && time < ect) {
            updateStarts(nUpdateStarts) = task
            nUpdateStarts += 1
          } else if (lst <= time && time < lct) {
            updateEnds(nUpdateEnds) = task
            nUpdateEnds += 1
          }
        }

        // Checker
        if (gap < 0) {
          // Fail
          builder.clear()
          var i = nMandatories
          while (i > 0) {
            i -= 1
            val t = mandatories(time)(i)
            val lit = overlaps(time)(t)
            builder.add(-lit)
          }
          if (!cdclStore.addExplanationClause(builder.toArray)) fail = true
        } // Explain
        else {

          // Update starts 
          while (nUpdateStarts > 0) {
            nUpdateStarts -= 1
            val task = updateStarts(nUpdateStarts)
            val demand = demands(task)
            if (demand > gap) {
              builder.clear()
              var i = nMandatories
              while (i > 0) {
                i -= 1
                val t = mandatories(time)(i)
                val lit = overlaps(time)(t)
                builder.add(-lit)
              }
              builder.add(-starts(task).greaterEqual(time - durations(task) + 1))
              builder.add(starts(task).greaterEqual(time + 1))
              if (!cdclStore.addExplanationClause(builder.toArray)) fail = true
            }
          }

          // Update ends 
          while (nUpdateEnds > 0 && !fail) {
            nUpdateEnds -= 1
            val task = updateEnds(nUpdateEnds)
            val demand = demands(task)
            if (demand > gap) {
              builder.clear()
              var i = nMandatories
              while (i > 0) {
                i -= 1
                val t = mandatories(time)(i)
                val lit = overlaps(time)(t)
                builder.add(-lit)
              }
              builder.add(-starts(task).lowerEqual(time))
              builder.add(starts(task).lowerEqual(time - durations(task)))
              if (!cdclStore.addExplanationClause(builder.toArray)) fail = true
            }
          }
        }

        // Update reversibles    
        nActivesRev(time).value = nActives
        gapRev(time).value = gap
        nMandatoriesRev(time).value = nMandatories
      }
    }

    if (fail) Failure
    else Suspend
  }

  @inline private def deactivate(time: Int, id: Int): Unit = {
    nActives -= 1
    val tmp = actives(time)(id)
    actives(time)(id) = actives(time)(nActives)
    actives(time)(nActives) = tmp
  }
}