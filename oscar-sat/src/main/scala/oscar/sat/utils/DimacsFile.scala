package oscar.sat.utils

import scala.io.Source
import oscar.sat.core.CDCLStore
import oscar.sat.heuristics.ActivityHeuristic

class DimacsFile(final val nClauses: Int, final val nVariables: Int, final val clauses: Array[Array[Int]]) {
  
  final def model: CDCLStore = {
    val solver = new CDCLStore()
    // Creates literals
    val literals = Array.tabulate(nVariables)(i => solver.newVar((i+1).toString))
    // Creates clauses
    Array.tabulate(nClauses)(c => {
      val clause = clauses(c).map(i => {
        if (i < 0) (-i-1) * 2 + 1
        else (i - 1) * 2
      })
      solver.newClause(clause)
    })
    // Return the model
    solver
  }
  
  final def verify(assignment: Array[Boolean]): Boolean = {
    var i = 0
    while (i < clauses.length) {
      val clause = clauses(i)
      var j = 0
      var satisfied = false
      while (j < clause.length) {
        val lit = clause(j)
        val signed = lit < 0
        val id = if (signed) -lit - 1 else lit - 1
        val value = if (signed) !assignment(id) else assignment(id)
        satisfied |= value
        j += 1
      }
      if (satisfied == false) {
        println("clause " + i + " is not satisfied")
        return false
      }
      i += 1
    }
    true
  }
}

object DimacsFile {
  
  final def parse(dimacsFile: String): DimacsFile = {
    val lines = Source.fromFile(dimacsFile).getLines
    if (lines.isEmpty) null
    else {
      // Data
      var nClauses = 0
      var nVariables = 0
      // Comment lines
      var comments = true
      while (lines.hasNext && comments) {
        val line = lines.next.replaceAll("\\s+", " ")
        if (line.charAt(0) == 'p') {
          val data = line.trim.split("\\s+")
          nVariables = data(2).toInt
          nClauses = data(3).toInt
          comments = false
        }
      }
      // Problem data
      val clauses = new Array[Array[Int]](nClauses)
      var i = 0
      while (i < nClauses) {
        val line = lines.next.replaceAll("\\s+", " ")
        val data = line.trim.split("\\s+").dropRight(1)
        clauses(i) = data.map(_.toInt)
        i += 1
      }
      new DimacsFile(nClauses, nVariables, clauses)
    }
  }
}