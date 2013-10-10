package oscar.cp.dsl.instances

import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class AircraftLandingInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbPlanes, freezeTime) = readLine asInt
  
  val planeValues = 6 + nbPlanes 	// Planes are described by 6 values plus an array of nbPlanes values.
  
  val datas = readDatas(nbPlanes, planeValues)
  
  // First 4 columns, alongside the plane number, are integers.
  val Array(
      	planes,
      	appearanceTimes, 
      	earliestLandingTimes, 
      	targetLandingTimes, 
      	latestLandingTimes
      ) = datas.slice(0, 5) asInt
      
  // Next two ones are costs and are doubles.    
  val Array(
      	costsForEarlyLanding, 
      	costsForLateLanding
      ) = datas.slice(5, 7) asDouble
      
  // The last columns are actually a matrix of integers.    
  val separationTimes: Array[Array[Int]] = datas.slice(7, datas.length).asInt transpose
  
  val cp = CPScheduler(latestLandingTimes.sum)

}