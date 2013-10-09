package oscar.cp.test.scheduling

import oscar.cp.dsl.instances.AircraftLandingInstance

object MyAircraftLanding extends AircraftLandingInstance("data/memScheduling/aircraft-landing/airland1.txt") with App{

  					planes.map(println)
      			appearanceTimes.map(println) 
      			earliestLandingTimes.map(println) 
      			targetLandingTimes.map(println)
      			latestLandingTimes.map(println)
      			costsForEarlyLanding.map(println)
      			costsForLateLanding.map(println)
  
}