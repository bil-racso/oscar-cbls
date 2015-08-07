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
package oscar.examples.cp

import scala.util.Random
import oscar.cp._

/**
 * In a hospital, there is a certain number of rooms, and a certain number of doctors, with special skills.
 * Each day, a doctor may give care with their skills in a room of the hospital.
 * There is a limited number of rooms to give care, all the doctors cannot work on the same day.
 * Furthermore, each day, there may be a minimum service in some skills that the hospital has to ensure.
 * Every doctor will work a certain number of days bounded below and above according to their preference.
 *
 * Following these constraints, the hospital wants to maximize the total care, that is, the sum of all the days every
 * one of the doctors worked.
 *
 * @author Victor Lecomte
 */
object Hospital extends MultiGCCBenchmark {

  override def run() {

    val rand = new Random(seed)

    val nDoctors = 6
    val nSkills = 4
    val nDays = 8
    val nRooms = 4

    val maxSkills = nSkills/2
    val minServicePerDay = 2*nRooms/3
    val maxMinWork = nDays/2
    val minMaxWork = nDays/2-1

    val skillDoNothing = -1

    val rDoctors = 0 until nDoctors
    val rSkills = 0 until nSkills
    val rAllSkills = skillDoNothing until nSkills
    val rDays = 0 until nDays
    val rDoNothing = skillDoNothing to skillDoNothing

    // Assign skills to doctors
    var skills: Array[Set[Int]] = null
    var nDoctorsForSkill: Array[Int] = null
    var nAssignedSkills = 0
    do {
      skills = Array.fill(nDoctors)(Set())
      val assigned: Array[Boolean] = Array.fill(nSkills)(false)
      val doctorsForSkill: Array[Set[Int]] = Array.fill(nSkills)(Set())

      for (distribution <- 0 until maxSkills) {
        for (doctor <- rDoctors) {
          val skill = rand.nextInt(nSkills)
          skills(doctor) += skill
          doctorsForSkill(skill) += doctor
          if (!assigned(skill)) {
            assigned(skill) = true
            nAssignedSkills += 1
          }
        }
      }
      nDoctorsForSkill = doctorsForSkill.map(_.size)
    } while (nAssignedSkills != nSkills) // Make sure all skills are assigned to at least one doctor

    // Determine the minimum service for each skill, each day.
    val minServiceFor: Array[Array[Int]] = Array.fill(nDays, nSkills)(0)
    for (day <- rDays) {
      for (i <- 0 until minServicePerDay) {
        var skill = 0
        // Avoid situations where the minimum service is larger than the number of doctors for that skill
        do { skill = rand.nextInt(nSkills) }
        while (nDoctorsForSkill(skill) <= minServiceFor(day)(skill))
        minServiceFor(day)(skill) += 1
      }
    }

    // Determine the minimal and maximal number of work days for each doctor
    val minDays = Array.fill(nDoctors)(rand.nextInt(maxMinWork))
    val maxDays = Array.fill(nDoctors)(rand.nextInt(nDays-minMaxWork) + minMaxWork)

    // Occupation of a certain doctor on a certain day
    val occupation: Array[Array[CPIntVar]] = Array.tabulate(nDoctors, nDays)
    { (doctor, day) => CPIntVar(skills(doctor) + skillDoNothing) }

    // Each doctor must work at least minDays(doctor) and at most maxDays(doctor)
    for (doctor <- rDoctors) {
      add(gcc(occupation(doctor), rDoNothing, nDays - maxDays(doctor), nDays - minDays(doctor)))

      /* Attempt at a fix for BC...
      val lower = rAllSkills.map {
        case `skillDoNothing` => nDays - maxDays(doctor)
        case _ => 0
      }
      val upper = rAllSkills.map {
        case `skillDoNothing` => nDays - minDays(doctor)
        case _ => nDays
      }
      add(gcc(occupation(doctor), rAllSkills, lower, upper))
      */
    }

    for (day <- rDays) {
      // Each doctor's occupation today
      val occupationsToday = occupation.map(occ => occ(day))

      val lower = rAllSkills.map {
        // There must not be more active doctors than rooms in the hospital
        case `skillDoNothing` => nDoctors - nRooms
        // Each skill must have its minimum service performed
        case skill => minServiceFor(day)(skill)
      }.toArray
      // No upper bound
      val upper = rAllSkills.map(_ => nDoctors).toArray

      add(gcc(occupationsToday, rAllSkills, lower, upper))
    }

    // Maximizing the total care is equivalent to minimizing the total rest
    val totalRest = CPIntVar(0 to nDays*nRooms)
    add(countEq(totalRest, occupation.flatten, skillDoNothing))
    minimize(totalRest)

    search { binaryFirstFail(occupation.flatten[CPIntVar]) }
    onSolution {
      for (doctor <- rDoctors) {
        println("(" + minDays(doctor) + "-" + maxDays(doctor) + ") " +
          occupation(doctor).map(x => if (x.min == skillDoNothing) "x" else x.min).mkString(" ") +
          " (" + skills(doctor).mkString(" ") + ")")
      }
    }

    print(start(failureLimit = 2000000))
  }
}