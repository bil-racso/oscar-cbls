package oscar.examples.cp.memScheduling

import oscar.cp.memScheduling.instances.AircraftLandingInstance
import scala.Array.canBuildFrom

object MyAircraftLanding extends AircraftLandingInstance("data/memScheduling/aircraft-landing/airland9.txt") with App{

  					planes.map(println)
      			appearanceTimes.map(println) 
      			earliestLandingTimes.map(println) 
      			targetLandingTimes.map(println)
      			latestLandingTimes.map(println)
      			costsForEarlyLanding.map(println)
      			costsForLateLanding.map(println)
      			separationTimes(99).map(println)
      			
      			// TODO: Well, now... model this!
}