package oscar.cbls.invariants.core.algo.fun


class UpdateableBijection extends UpdateableFunction{

  val reverseFunction:UpdateableFunction = new UpdateableFunction

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
}

sealed abstract class Change
case class InsertChange(e:EnrichedPivot) extends Change
case class DeleteChange(e:EnrichedPivot) extends Change
case class UpdateChange(e:EnrichedPivot) extends Change

class EnrichedPivot(override val value:Int,
                    override var next:Pivot = null,
                    override var prev:Pivot,
                    override var f: LinearPositionTransform)
  extends Pivot(value:Int, next:Pivot, prev:Pivot, f){
  var prevF:LinearPositionTransform = null
  var counterpart:Pivot = null
}

