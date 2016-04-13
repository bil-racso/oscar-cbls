package oscar.cbls.invariants.core.algo.fun.mutable


class UpdateableBijectionNaive extends PiecewiseLinearFunction{
  val reverseFunction:PiecewiseLinearFunction = new PiecewiseLinearFunction

  private var upToDate = true

  def unApply(value:Int):Int = {
    if(!upToDate) buildReverse()
    reverseFunction.apply(value)
  }

  def buildReverse(){
    if(upToDate) return
    reverseFunction.setAsIdentity()
    this.firstPivot match{
      case Some((_,p)) =>
        reverseFunction.clearAndSetPivots(computeInvertedPivots(p))
    }
    upToDate = true
  }

  private def computeInvertedPivots(p:Pivot, newPivots:List[Pivot] = List.empty):List[Pivot] = {
    if(p == null) return newPivots
    val reverseF = p.f.invert
    val firstValueAsPivot = !p.f.minus
    val value = if(firstValueAsPivot) p.value else p.next.value
    computeInvertedPivots(p.next, new Pivot(value,null,null,reverseF) :: newPivots)
  }
}

class UpdateableBijection extends PiecewiseLinearFunction{



  val reverseFunction:PiecewiseLinearFunction = new PiecewiseLinearFunction

  // UpdateableFunction
  def unApply(value:Int):Int = reverseFunction(value)

  override def update(fromIncluded: Int, toIncluded: Int, additionalF: LinearPositionTransform): Unit =
    super.update(fromIncluded, toIncluded, additionalF)

  var changes:List[Change] = List.empty

  override protected def updatedPivot(p: Pivot){
    changes = new UpdateChange(p.asInstanceOf[EnrichedPivot]) :: changes
  }

  override protected def deletedPivot(p: Pivot){
    changes = new DeleteChange(p.asInstanceOf[EnrichedPivot]) :: changes
  }

  override protected def insertedPivot(p: Pivot){
    changes = new InsertChange(p.asInstanceOf[EnrichedPivot]) :: changes
  }

  override protected def createNewPivot(value : Int, next : Pivot, prev : Pivot, f : LinearPositionTransform) : Pivot =
    new EnrichedPivot(value,next,prev,f)

  def performReverseUpdate(){
    for(change <- changes){
      change match{
        case InsertChange(p) =>
        //also handle update case here!
        case DeleteChange(p) =>
          //update the pivot Before the reverse
          if(p.prevF.minus){
            //was a negative slope
          }

        case UpdateChange(p) =>
      }
    }
  }

  sealed abstract class Change
  case class InsertChange(e:EnrichedPivot) extends Change
  case class DeleteChange(e:EnrichedPivot) extends Change
  case class UpdateChange(e:EnrichedPivot) extends Change

  class EnrichedPivot(override val value:Int,
                      next:Pivot = null,
                      prev:Pivot,
                      f: LinearPositionTransform)
    extends Pivot(value:Int, next:Pivot, prev:Pivot, f){
    var prevF:LinearPositionTransform = null
    var counterpart:Pivot = null
  }
}

