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

package oscar.des.montecarlo

import scala.collection.mutable
import oscar.des.engine.NumberGenerator
import JSci.maths.statistics._

/**
 * These classes represent Integer and Double Random Variables which have probability
 * distributions.
 *
 * Created by go on 26/11/2014.
 */

/**
 * Case classes representing the actual type of random variables
 */
abstract class TypeRdVar
case object IntRdVar extends TypeRdVar
case object DoubleRdVar extends TypeRdVar

/**
 * Abstract class which encapsulates the behaviour of random variables
 * @param name The name
 * @param numGen The number generator, associated to the probability distribution
 * @author gustavo.ospina@cetic.be
 */
abstract class RandomVar(name : String, numGen : NumberGenerator) {

  def setValue()

  def getValue : AnyVal

  def intRandomFunc : () => Int

  def doubleRandomFunc : () => Double
}

/**
 * Integer Random Variable
 * @param name The name
 * @param numGen The number generator, associated to the probability distribution
 * @author gustavo.ospina@cetic.be
 */
class IntRandomVar(name : String, numGen : NumberGenerator) extends RandomVar(name, numGen) {
  private var value : Option[Int] = None
  val typeVar = IntRdVar

  def this(nm : String, dist : ProbabilityDistribution) = this(nm, new NumberGenerator(dist))

  def setValue(): Unit = {
    value = Some(numGen.generateNext.toInt)
  }

  def getValue : Int = value match {
    case Some(x) => x
    case None =>
      val x = numGen.generateNext.toInt
      value = Some(x)
      x
  }

  def intRandomFunc = () => getValue

  def doubleRandomFunc = () => getValue

  override def toString : String = {
    val strVal = value match {
      case Some(x) => x.toString
      case None => "No Value"
    }
    "Int Random Variable " + name + " = " + strVal
  }
}

/**
 * Double Random Variable
 * @param name The name
 * @param numGen The number generator, associated to the probability distribution
 * @author gustavo.ospina@cetic.be
 */
class DoubleRandomVar(name : String, numGen : NumberGenerator) extends RandomVar(name, numGen) {
  private var value : Option[Double] = None
  val typeVar = DoubleRdVar

  def this(nm : String, dist : ProbabilityDistribution) = this(nm, new NumberGenerator(dist))

  def setValue(): Unit = {
    value = Some(numGen.generateNext)
  }

  def getValue : Double = value match {
    case Some(x) => x
    case None =>
      val x = numGen.generateNext
      value = Some(x)
      x
  }

  def intRandomFunc = () => getValue.toInt

  def doubleRandomFunc = () => getValue

  override def toString : String = {
    val strVal = value match {
      case Some(x) => x.toString
      case None => "No Value"
    }
    "Double Random Variable " + name + " = " + strVal
  }
}

/**
 * This class represents a list of Random Variables which have all the same
 * probability distribution. That means they all share the same number
 * generator (maybe this is not strictly correct)
 *
 * @param numGen The number generator associated to all variables
 * @author gustavo.ospina@cetic.be
 */
class RandomVarList(numGen : NumberGenerator) {
  private var rvList : List[RandomVar] = List()

  def this(dist : ProbabilityDistribution) = this(new NumberGenerator(dist))

  // I prepend, instead of append, like in declarative lists, because it's much
  // more efficient. If it is important to preserve the order of insertion, we
  // should consider to reverse the list.
  def addIntRandomVar(nameVar : String): Unit = {
    //rvList = rvList ::: List(new IntRandomVar(nameVar, numberGenerator))
    rvList = new IntRandomVar(nameVar, numGen) :: rvList
  }
  def addDoubleRandomVar(nameVar : String): Unit = {
    //rvList = rvList ::: List(new DoubleRandomVar(nameVar, numberGenerator))
    rvList = new DoubleRandomVar(nameVar, numGen) :: rvList
  }

  def setValues(): Unit = { rvList.foreach(rv => { rv.setValue() }) }

  def getValues : List[AnyVal] = rvList.map(rv => rv.getValue)

  def getRVList : List[RandomVar] = rvList

  override def toString : String = {
    ("Random Vars :\n" /: rvList) ((s, rv) => s + rv + "\n")
  }
}

/**
 * This class represents a set of Random Variables. Notice that variables in the set
 * can have different probability distributions.
 *
 * @author gustavo.ospina@cetic.be
 */
class RandomVarSet {
  private var rvSet : mutable.Set[RandomVar] = mutable.Set()

  def addIntRandomVar(nameVar : String, dist : ProbabilityDistribution): Unit = {
    rvSet += new IntRandomVar(nameVar, dist)
  }

  def addDoubleRandomVar(nameVar : String, dist : ProbabilityDistribution): Unit = {
    rvSet += new DoubleRandomVar(nameVar, dist)
  }

  def addRandomVarList(rvLst : RandomVarList): Unit = {
    rvLst.getRVList.foreach((rv) => { rvSet += rv })
  }

  def addRandomVarSet(otherSet : RandomVarSet): Unit = {
    rvSet = rvSet ++ otherSet.rvSet
  }

  def setValues(): Unit = { rvSet.foreach(rv => { rv.setValue() }) }

  def getValues : List[AnyVal] = rvSet.map(rv => rv.getValue).toList
}