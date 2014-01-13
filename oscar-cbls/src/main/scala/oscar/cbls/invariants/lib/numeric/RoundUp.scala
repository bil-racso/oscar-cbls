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
package oscar.cbls.invariants.lib.numeric

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.{Store, IntInvariant, IntVar}
import oscar.cbls.invariants.lib.logic.LazyIntVarIntVar2IntVarFun
import oscar.cbls.invariants.core.propagation.Checker

/**Maintains output to the smallest value such that
 * output >= from
 * (output - shift) MOD period > zone
 * (output - shift + length) MOD period > zone
 * of course, it is required that length is < period - zone, and exception is thrown otherwise.
 *
 * For instance, suppose that some task can only happen during open day (Mon-Fri),
 * let 'from" being the lowest starting date, and 'length' its duration.
 * the invariant will check that the task can be finished by friday of the week, and if not,
 * will propose the next monday. 'shift' specifies says what is the starting day at zero.
 * zone is the forbidden zone. it starts at the beginning of the cycle.
 *
 * Suppose you represent days starting from zero, and zero is a monday,
 * and you want to round up to the next open day (sa and su are closed day, the correct declaration is:
 * RoundUpModulo(from,duration,7,2,5)
 *
 * @param from the starting date of the task. it can start later.
 * @param duration the duration of the task.
 * @param period the period of the forbidden-allowed pattern
 * @param zone the size of the forbidden zone. it starts at the beginning of the period
 * @param shift the first period starts later than zero. it starts at shift. the duration before its start is allowed.
 */
 case class RoundUpModulo(from: IntVar, duration: IntVar, period: Int, zone: Int, shift: Int)
  extends LazyIntVarIntVar2IntVarFun(from, duration, (from: Int, duration: Int) => {
    require(duration <= period - zone, "duration " + duration + "<= period " + period  + "- zone " + zone)
    require(period != 0)
    val reducedfrom = (from + period - shift) % period
    if (reducedfrom < zone)
      from + (zone - reducedfrom) //to restore the modulo, we must compute this
    else if (reducedfrom + duration > period)
      from + (period + zone - reducedfrom)
    else
      from
  }, from.minVal, from.maxVal + zone){
}

object testRoundUpModulo extends App{

  def n2day(n:Int):String =
  n % 7 match{
    case 0 => "lu"
    case 1 => "ma"
    case 2 => "me"
    case 3 => "je"
    case 4 => "ve"
    case 5 => "sa"
    case 6 => "di"

  }
  val m = new Store()

  val from = IntVar(m, 0,"from")
  val duration = IntVar(m, 2,"duration")

  val r = RoundUpModulo(from,duration,7,2,0).toIntVar

  m.close()

  for(i <- 0 to 20){
    from := i
    println(n2day(from.value) + " " + from.value + " " + duration + " " + n2day(r.value) + " " + r.value)

  }
}

/**Maintains output to the smallest value such that
  * output >= from
  * the interval [output ; output + length] does not overlap with the intervals given in FobiddenZones
  *
  *Warning: the duration should never be zero.
  *
 * @param from
 * @param duration
 * @param ForbiddenZones
 */
case class RoundUpCustom(from: IntVar, duration: IntVar, ForbiddenZones: List[(Int, Int)]) extends IntInvariant {

  def myMax = ForbiddenZones.maxBy(_._2)._2 + 1

  def myMin = from.minVal

  var output: IntVar = null
  registerStaticAndDynamicDependenciesNoID(from, duration)
  finishInitialization()

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := roundup()
  }

  @inline
  override def notifyIntChanged(v: IntVar, OldVal: Int, NewVal: Int) {
    scheduleForPropagation()
  }

  override def performPropagation() {
    output := roundup()
  }

  private def regularizeZones(zones:List[(Int,Int)]):List[(Int,Int)] = {
    zones match{
      case List(head) => zones
      case (a,b) :: tail => {
        assert(a <= b)
        absorb(a,b,regularizeZones(tail))
      }
      case Nil => zones
    }
  }

  private def absorb(a:Int,b:Int,list:List[(Int,Int)]):List[(Int,Int)] = {
    list match{
      case (c,d) :: tailTail if(b >= c) =>  absorb(a,(d max b), tailTail)
      case newTail => (a,b) :: newTail
    }
  }

  val SortedRegularizedZones = regularizeZones(ForbiddenZones.sortBy(_._1))

  val ForbiddenStarts: Array[Int] = SortedRegularizedZones.map(_._1).toArray
  val ForbiddenEnds: Array[Int] = SortedRegularizedZones.map(_._2).toArray

  def roundup(): Int = {
    var NewStart: Int = from.value
    var LastZoneBeforeNewStart = FindLastStartBefore(from.value)
    while (true) {
      if (LastZoneBeforeNewStart != -1 && ForbiddenEnds(LastZoneBeforeNewStart) >= NewStart) {
        //we cannot start here, we are in a forbidden zone
        //the zone before start does not change, we just need to wait for the end of this zone
        NewStart = ForbiddenEnds(LastZoneBeforeNewStart) + 1
      } else if ((LastZoneBeforeNewStart + 1 < ForbiddenStarts.size) && (ForbiddenStarts(LastZoneBeforeNewStart + 1) <= NewStart + duration.value -1)) {
        //we can start here, but we end up in a forbidden zone
        //we have to start after the next zone
        NewStart = ForbiddenEnds(LastZoneBeforeNewStart + 1) + 1
        LastZoneBeforeNewStart += 1
      } else {
        return NewStart
      }
    }
    1
  }

  def FindLastStartBefore(d: Int): Int = {
    var up = ForbiddenStarts.size - 1
    var down = -1
    while (down + 1 < up) {
      val mid = (up + down) / 2
      if (ForbiddenStarts(mid) == d) {
        return mid
      } else if (ForbiddenStarts(mid) < d) {
        down = mid
      } else {
        up = mid
      }
    }
    if (ForbiddenStarts(up) <= d) up
    else down
  }

  override def checkInternals(c: Checker){
    c.check(from.value <= output.value)
    for((a,b) <- ForbiddenZones){
      c.check((output.value > b) || (output.value + duration.value -1 < a), Some("from.value = " + from.value + " (output.value " + output.value + " > zoneEnd " + b + ") || (output.value " + output.value + "+ duration.value " + duration.value + " -1 < zoneStart " + a + ")"))
    }

    for(i <- from.value until output.value){
      c.check(ForbiddenZones.exists(ab => !((i + duration.value -1 < ab._1) || (ab._2 < i))),Some("should be not suitable at position " + i + " " + duration + " exists:" + ForbiddenZones.exists(ab => !((i + duration.value -1 < ab._1) || (ab._2 < i))) + "filter:" + ForbiddenZones.filter(ab => !((i + duration.value -1 < ab._1) || (ab._2 < i)))))
    }
  }
}


object testRoundUpCustom extends App{

  def n2day(n:Int):String =
    n % 7 match{
      case 0 => "lu"
      case 1 => "ma"
      case 2 => "me"
      case 3 => "je"
      case 4 => "ve"
      case 5 => "sa"
      case 6 => "di"

    }
  val m = new Store()

  val from = IntVar(m, 0,"from")
  val duration = IntVar(m, 2,"duration")

  val r = RoundUpCustom(from,duration,List((3,4), (9,12))).toIntVar

  m.close()

  for(i <- 0 to 20){
    from := i
    println(n2day(from.value) + " " + from.value + " " + duration + " " + n2day(r.value) + " " + r.value)
  }
}

