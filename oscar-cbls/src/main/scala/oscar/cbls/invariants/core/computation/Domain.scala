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
  *     This code has been initially developed by
  *         by Jean Noël Monette
  ******************************************************************************/
package oscar.cbls.invariants.core.computation

import scala.collection.immutable.SortedSet
import scala.util.Random
import scala.language.implicitConversions

object Domain{
  implicit def rangeToDomain(r:Range):Domain = {
    if (r.step == 1) DomainRange(r.head,r.last)
    else DomainSet(r.toSet)
  }

  implicit def coupleToDomain(i:(Int,Int)):Domain = {
    if(i._1 == i._2) SingleValueDomain(i._1)
      else DomainRange(i._1,i._2)
  }

  implicit def setToDomain(s:Set[Int]):Domain = {
    if (s.size == 1) SingleValueDomain(s.head)
    else DomainSet(s)
  }


  def apply(v:Iterable[Int]):Domain =
  v match{
    case s:Set[Int] => s
    case r:Range => r
    case i:Iterable[Int] => SortedSet.empty[Int] ++ i
  }
}

sealed abstract class Domain {
  def min: Int
  def max: Int
  def size: Int
  def contains(v:Int): Boolean
//  def restrict(d:Domain):Domain
  def values:Iterable[Int]
  def randomValue:Int
  def restrict(d:Domain):Domain

  def union(d:Domain):Domain = throw new Error("not implemented")
  def inter(d:Domain):Domain = throw new Error("not implemented")
}

/**this is an inclusive domain*/
case class DomainRange(override val min: Int, override val max: Int) extends Domain {
  if (min > max) throw new EmptyDomainException
  def contains(v:Int): Boolean = min <= v && max >= v
  def size = if(max==Int.MaxValue && min==Int.MinValue) Int.MaxValue else math.max(max-min+1,0)
  override def values: Iterable[Int] = min to max
  override def randomValue: Int = (min to max)(Random.nextInt(max-min+1))
  override def restrict(d: Domain): Domain = {
    d match{
      case r:DomainRange => math.max(r.min,min) to math.min(r.max,max)
      case s:DomainSet => s.values.--(values)
      case FullRange => d
      case d:SingleValueDomain => d.restrict(this)
    }
  }
}

case class DomainSet(values: Set[Int]) extends Domain {
  if (values.isEmpty) throw new EmptyDomainException
  def min = values.min
  def max = values.max
  def size = values.size
  def contains(v:Int): Boolean = values.contains(v)
  override def randomValue: Int = values.toIndexedSeq(Random.nextInt(values.size))
  override def restrict(d: Domain): Domain = d.restrict(this)
}

case object FullRange extends Domain{
  override def min: Int = Int.MinValue
  override def max: Int = Int.MaxValue
  override def size: Int = Int.MaxValue
  override def randomValue: Int = Random.nextInt()
  override def contains(v: Int): Boolean = true
  override def values: Iterable[Int] =  min to max
  override def restrict(d: Domain): Domain = d
}

case class SingleValueDomain(value:Int) extends Domain{
  override def min: Int = value
  override def max: Int = value
  override def size: Int = 1
  override def contains(v: Int): Boolean = v == value

  override def randomValue: Int = value

  override def restrict(d: Domain): Domain =
    if (d.contains(value)) this else throw new EmptyDomainException
  override def values: Iterable[Int] = List(value)
}

class EmptyDomainException extends Exception("domain is empty")