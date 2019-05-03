package oscar.cbls.business.geometry.invariants

import org.locationtech.jts.algorithm.distance.{DistanceToPoint, PointPairDistance}
import org.locationtech.jts.geom.prep.PreparedGeometryFactory
import oscar.cbls.business.geometry.algo.MaxDistance
import oscar.cbls.business.geometry.model.{CBLSGeometryConst, CBLSGeometryVar, GeometryNotificationTarget, GeometryValue}
import oscar.cbls.core.IntInvariant
import oscar.cbls.core.computation.ChangingAtomicValue

/**
  *
  * @param innerShape
  * @param outerShape
  */
class StrictInclusion(innerShape:CBLSGeometryVar, outerShape:CBLSGeometryConst, violationIsDistanceBetweenCenters:Boolean)
  extends IntInvariant
    with GeometryNotificationTarget{

  registerStaticAndDynamicDependency(innerShape, i = 0)
  registerStaticAndDynamicDependency(outerShape, i = 1)
  finishInitialization()

  require(outerShape.value.geometry.isSimple)
  private val preparedOuterGeometry = PreparedGeometryFactory.prepare(outerShape.value.geometry)

  scheduleForPropagation()

  //violation = la distance entre le le centre de la figure outer et le point le plus éloigné de ce centre
  //ATTENTION: ne supportera pas les figures convexes ou en plusieurs morceaux

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue],
                                    id: Int,
                                    oldVal: GeometryValue,
                                    newVal: GeometryValue): Unit = {
    require(id == 1)

    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {

    if(preparedOuterGeometry.containsProperly(innerShape.value.geometry)){
      //we are fine
      this := 0
    }else{
      //there is something that goes out of the outerShape...
      //we compute the distance between the center of the prepared and the center of the other?
      //distance between the center of the prepared and the

      if(violationIsDistanceBetweenCenters){
        //the outer is like a circle, so we take the distance between the two centers.
        //the distance between the two centers? no because a rotation might solve the problem, and such distance would not capture it in the violation.
        val ptDist = new PointPairDistance()
        DistanceToPoint.computeDistance(
          innerShape.value.centerOfOverApproximatingCircle,
          outerShape.value.centerOfOverApproximatingCircle.getCoordinate,
          ptDist) //in distance among all points of the geometry and other point
        this := ptDist.getDistance().toLong
      }else{
        //the longest distance between the centre of the shape and all points of the other shape
        this := MaxDistance.computeDistance(innerShape.value.geometry, outerShape.value.centerOfOverApproximatingCircle.getCoordinate).getDistance.toLong
      }
    }
  }
}


