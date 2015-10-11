package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.algebra._

object ProductionPlanning extends MPModel(MPSolver.lp_solve) with App {
  val b = Array(18209, 7692, 1333, 924, 26638, 61188, 13360) // Dimensions
  val c = Array(96, 76, 56, 11, 86, 10, 66, 86, 83, 12, 9, 81) // Products

  val Dimensions = 0 until b.size
  val Products = 0 until c.size

  val coef = Array(
    Array(19,   1, 10,  1,   1,  14, 152, 11,  1,   1, 1, 1), //Dimensions x Products
    Array( 0,   4, 53,  0,   0,  80,   0,  4,  5,   0, 0, 0),
    Array( 4, 660,  3,  0,  30,   0,   3,  0,  4,  90, 0, 0),
    Array( 7,   0, 18,  6, 770, 330,   7,  0,  0,   6, 0, 0),
    Array( 0,  20,  0,  4,  52,   3,   0,  0,  0,   5, 4, 0),
    Array( 0,   0, 40, 70,   4,  63,   0,  0, 60,   0, 4, 0),
    Array( 0,  32,  0,  0,   0,   5,   0,  3,  0, 660, 0, 9)
  )

  val x = Products.map(p => FloatVar("x", 0, 10000))

  maximize(sum(Products) { p => x(p) * c(p) })
  
  for (d <- Dimensions) {
    add(sum(Products)(p => coef(d)(p) * x(p)) <= b(d))
  }

  for (d <- Dimensions) {
    add(sum(Products)(p => coef(d)(p) * x(p)) <= b(d))
  }

  val endStatus = solver.solve

  println("objective: " + solver.objectiveValue)
  Products.foreach(p => if (x(p).value.get > 0) println("x" + p + ":" + x(p).value.get))

  solver.release()
}
