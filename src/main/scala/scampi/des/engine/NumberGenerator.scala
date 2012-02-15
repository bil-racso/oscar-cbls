package scampi.des.engine

import scala.Math._
import scala.util.Random
import scala.util.continuations._
import JSci.maths.statistics._

class NumberGenerator(dist: ProbabilityDistribution) {

  val generator = new Random()

  var generating = true

  def stop() { generating = false }

  def apply(): Double = dist.inverse(generator.nextDouble)
  def generateNext = apply

}