/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package oscar.des.engine

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
