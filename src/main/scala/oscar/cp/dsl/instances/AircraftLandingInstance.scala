package oscar.cp.dsl.instances

import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class AircraftLandingInstance(filepath: String) {
 
  val reader = new InstanceReader(filepath)
  
  val Array(nbPlanes, freezeTime) = reader.readLine
  val datas = reader.readDatas(16, 3) // Planes are described by 16 values spread on 3 lines
  
  val Array(
      	planes,
      	appearanceTimes, 
      	earliestLandingTimes, 
      	targetLandingTimes, 
      	latestLandingTimes, 
      	costsForEarlyLanding, 
      	costsForLateLanding
      ) = datas.dropRight(nbPlanes) // The last nbPlanes arrays are actually a matrix of planes and times, not simple arrays.
      			
  val separationTimes = datas.drop(datas.length - nbPlanes)	// Now we select the last nbPlanes arrays as a matrix.
  
  val cp = CPScheduler(latestLandingTimes.sum)

}