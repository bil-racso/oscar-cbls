package scampi.des.engine

import scala.Math._
import scala.util.Random
import scala.util.continuations._
import JSci.maths.statistics._
import scampi.invariants._

class Generator(m: Model, dist: ProbabilityDistribution, block: => Unit) extends NumberGenerator(dist){

  reset {
    while (generating) {
      val t = generateNext
      waitFor(m.clock === m.clock() + t)
      block
    }
  }
}