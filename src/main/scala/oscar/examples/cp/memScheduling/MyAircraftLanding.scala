package oscar.examples.cp.memScheduling

import oscar.cp.memScheduling._

object MyAircraftLanding extends App with Scheduler with Reader{
  
  read fromFile "data/memScheduling/aircraft-landing/airland9.txt"
	val Array(nbPlanes, freezeTime) = read fileFor 2 int
  // Planes are described by 6 values plus an array of nbPlanes values.
  val datas = read fileFor nbPlanes unitsOf 6+nbPlanes
  
  // First 4 columns, alongside the planes numbers, are integers.
  val Array(
      	planes,
      	appearanceTimes, 
      	earliestLandingTimes, 
      	targetLandingTimes, 
      	latestLandingTimes
      ) = datas extract 5 int
      
  // Next two ones are costs and are doubles.    
  val Array(
      	costsForEarlyLanding, 
      	costsForLateLanding
      ) = datas extract 2 double
      
  // The last columns are actually a matrix of integers.    
  val separationTimes: Array[Array[Int]] = datas extractMatrixOf nbPlanes int
  
  setHorizonTo(latestLandingTimes.sum)
  
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