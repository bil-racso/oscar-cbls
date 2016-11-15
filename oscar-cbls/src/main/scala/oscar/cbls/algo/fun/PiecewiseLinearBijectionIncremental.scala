package oscar.cbls.algo.fun

import oscar.cbls.algo.interval.Interval
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMapExplorer

object PiecewiseLinearBijectionIncremental{
  def identity:PiecewiseLinearBijectionIncremental = new PiecewiseLinearBijectionIncremental(PiecewiseLinearFun.identity)

  def apply(forward:PiecewiseLinearFun, backward:PiecewiseLinearFun):PiecewiseLinearBijectionIncremental = new PiecewiseLinearBijectionIncremental(forward,backward)
  def apply(forward:PiecewiseLinearFun) = new PiecewiseLinearBijectionIncremental(forward)

  def computeInvertedPivots(prevPivot : Pivot, remainingPivots : List[Pivot], newPivots : QList[Pivot] = null) : QList[Pivot] = {
    remainingPivots match {
      case Nil => newPivots
      case p1 :: p2 :: tail =>
        val fun = p1.f
        val newPivot = new Pivot(fun(if (fun.minus) p2.fromValue - 1 else p1.fromValue), fun.invert)
        computeInvertedPivots(p1, p2 :: tail, QList(newPivot, newPivots))
      case p1 :: tail =>
        val fun = p1.f
        require(!fun.minus)
        QList(new Pivot(fun(p1.fromValue), fun.invert), newPivots)
    }
  }
}

class PiecewiseLinearBijectionIncremental(val forward:PiecewiseLinearFun, val backward:PiecewiseLinearFun){

  def this(forward:PiecewiseLinearFun)= {
    this(forward,PiecewiseLinearFun.createFromPivots(PiecewiseLinearBijectionIncremental.computeInvertedPivots(null, forward.pivots, null)))
  }

  def invert:PiecewiseLinearBijectionIncremental = new PiecewiseLinearBijectionIncremental(backward,forward)

  def checkBijection() = ???

  override def toString : String = {
    "Bijection.forward: " + forward + "\n"+
      "Bijection.backward:" + backward + "\n"
  }


  private def updateReverseOnZone(backward:PiecewiseLinearFun,startZoneToUpdate:Int,endZoneToUpdate:Int,updatedForward:PiecewiseLinearFun):PiecewiseLinearFun = {
    val reversedPivotToInsert = updatedForward.pivotWithPositionApplyingTo(startZoneToUpdate) match{
      case None => //check if we are actually before the first pivot, so identity is considered here
        updatedForward.firstPivotAndPosition match{
          case None => //really nothing to do, this is identity everywhere
            (startZoneToUpdate,endZoneToUpdate,LinearTransform.identity) :: Nil
          case s@Some(e) =>
            //there is a pivot later on
            if(e.key<endZoneToUpdate) (startZoneToUpdate,e.key-1,LinearTransform.identity) :: getUpdatedReversedSegments(e.key,endZoneToUpdate,s)
            else (startZoneToUpdate,endZoneToUpdate,LinearTransform.identity)  :: Nil
        }
      case s@Some(_) => getUpdatedReversedSegments(startZoneToUpdate,endZoneToUpdate,s)
    }


    reversedPivotToInsert.foldLeft(backward)({
      case (backward, (startZoneToUpdate, endZoneToUpdate, f)) =>
        backward.setPivot(startZoneToUpdate, endZoneToUpdate, f)
    })
  }

  private def getUpdatedReversedSegments(startPositionIncluded:Int,endPositionIncluded:Int,explorerOptInForwardFunction:Option[RedBlackTreeMapExplorer[Pivot]]):List[(Int,Int,LinearTransform)] = {
    require(startPositionIncluded <= endPositionIncluded)
    explorerOptInForwardFunction match{
      case None =>
        //identity between startPositionIncluded and endPositionIncluded
        (startPositionIncluded,endPositionIncluded,LinearTransform.identity) :: Nil
      case Some(explorerInForwardFunction) =>
        val pivot = explorerInForwardFunction.value
        val fun = pivot.f
        val endOfIntervalIncluded = explorerInForwardFunction.next match{
          case None => endPositionIncluded
          case Some(nextExplorerOfPivot) =>
            if(nextExplorerOfPivot.key <= endPositionIncluded) nextExplorerOfPivot.key
            else endPositionIncluded
        }
        val newF = fun.invert

        val (reversedStartIntervalIncluded,reversedEndIntervalIncluded) = fun(startPositionIncluded,endOfIntervalIncluded)

        (reversedStartIntervalIncluded,reversedEndIntervalIncluded, newF) ::
          (if(endOfIntervalIncluded < endPositionIncluded){
            getUpdatedReversedSegments(endOfIntervalIncluded+1,endPositionIncluded,explorerInForwardFunction.next)
          }else List.empty[(Int,Int,LinearTransform)])
    }
  }

  def numberOfSegmentsInIntervals(intervals:Iterable[(Int,Int,LinearTransform)]):Int= {

    def pivotsUntilPassedEnd(explorerOpt:Option[RedBlackTreeMapExplorer[Pivot]],end:Int,acc:Int = 0):Int = {
      explorerOpt match{
        case None => acc
        case Some(e) =>
          if(e.key > end) acc
          else pivotsUntilPassedEnd(e.next,end,acc+1)
      }
    }

    intervals.foldLeft(0)({case (acc,(startInterval,endInterval,_)) =>
      acc + pivotsUntilPassedEnd(forward.pivotWithPositionApplyingTo(startInterval),endInterval)
    })
  }

  def updateBefore(updates:(Int,Int,LinearTransform)*):PiecewiseLinearBijectionIncremental ={

    val updatedForward = forward.updateForCompositionBefore(updates:_*)
    val maintainReverseIncrementally = true //(numberOfSegmentsInIntervals(updates)*10<forward.nbPivot)

    if(maintainReverseIncrementally){
      val startEnd:List[(Int,Int)] = updates.toList.map(update => (update._1,update._2))
      val sortedStartEnd = startEnd.sortBy(_._1)
      val zonesWhereReverseMustBeUpdated = Interval.mergeOverlappingIntervals(sortedStartEnd)
      val updatedBackward = zonesWhereReverseMustBeUpdated
        .foldLeft(backward)({
          case (backward,(startZoneToUpdate,endZoneToUpdate)) =>
            updateReverseOnZone(backward,startZoneToUpdate,endZoneToUpdate,updatedForward)})
      require(updatedBackward equals PiecewiseLinearFun.createFromPivots(PiecewiseLinearBijectionIncremental.computeInvertedPivots(null, updatedForward.pivots, null)),"error in incremental backward update: got:\n " + updatedBackward + "\n should be:\n" + PiecewiseLinearFun.createFromPivots(PiecewiseLinearBijectionIncremental.computeInvertedPivots(null, updatedForward.pivots, null)))
      new PiecewiseLinearBijectionIncremental(updatedForward,updatedBackward)
    }else{
      new PiecewiseLinearBijectionIncremental(updatedForward)
    }
  }

  def apply(value:Int) = forward(value)
  def unApply(value:Int) = backward(value)
}
