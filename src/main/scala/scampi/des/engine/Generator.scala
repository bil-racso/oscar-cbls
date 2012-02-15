package scampi.des.engine

import scala.Math._
import scala.util.Random
import scala.util.continuations._
import JSci.maths.statistics._

class Generator(m: Model, dist: ProbabilityDistribution, block: => Unit) extends NumberGenerator(dist){

  reset {
    while (generating) {
      m.wait(generateNext)
      block
    }
  }
}