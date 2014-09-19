package oscar.flatzinc.cbls.support
import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.objective.{ Objective => CBLSObjective }
import oscar.cbls.invariants.lib.logic.Cluster
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.objective.{Objective => CBLSObjective}
import oscar.cbls.search.SearchEngine




abstract class Move(val value:Int){
  def commit():Unit
  def getModified: Set[CBLSIntVar]
}
class AssignMove(x: CBLSIntVar,k:Int,value:Int) extends Move(value){
  def commit(){x := k}
  def getModified=Set(x)
  override def toString() = x + " assigned to " +k
}
class AssignsMove(xk: List[(CBLSIntVar,Int)],value:Int) extends Move(value){
  def commit(){xk.foreach(x => x._1 := x._2)}
  def getModified = xk.map(_._1).toSet
  override def toString() = xk.foldLeft("")((acc,x) => acc+x._1+ " assigned to " +x._2 +"; ")
}
class SwapMove(x: CBLSIntVar,y:CBLSIntVar,value:Int) extends Move(value){
  def commit(){x :=: y}
  def getModified=Set(x,y)
  override def toString() = x + " swapped with " +y
}
class NoMove(value:Int) extends Move(value){
  def commit(){}
  def getModified = Set.empty[CBLSIntVar]
  override def toString() = "No-Op"
}
class BeforeMove(m: Move,act:()=>Unit) extends Move(m.value) {
  def commit(){
    act()
    m.commit()
  }
  def getModified = m.getModified
  override def toString() = m.toString()
}






//Extends SearchEngine to access the selectors
abstract class Neighbourhood(val searchVariables: Array[CBLSIntVarDom]) extends SearchEngine {
  //var minObjective: Int = Int.MaxValue;
  def getVariables(): Array[CBLSIntVarDom] = searchVariables
  
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move;
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move;
  def randomMove(it: Int): Move;
  
 // def init(): Unit;
  def reset(): Unit;
  def violation(): Int;//TODO used in only one place. Useful?
}

class GCCNeighborhood(val variables: Array[CBLSIntVarDom],val vals:Array[Int],val low:Array[Int],val up:Array[Int], val closed:Boolean, objective: CBLSObjective, cs: ConstraintSystem)extends Neighbourhood(variables){
  val variableViolation: Array[CBLSIntVar] = variables.map(cs.violation(_)).toArray

  val counts = vals.foldLeft(Map.empty[Int,CBLSIntVar])((map,ic) => map + (ic -> new CBLSIntVar(cs._model,0 to variables.length,0,"count"+ic)))
  Cluster.MakeSparse(variables.map(c => c), vals).Clusters.foreach(ic => counts(ic._1) <== Cardinality(ic._2))
  //foldLeft(Map.empty[Int,CBLSIntVar])((map,ic) => map + (ic._1 -> Cardinality(ic._2).output))
  val lows = vals.toList.zip(low).foldLeft(Map.empty[Int,Int])((map,vl) => map + (vl._1 -> vl._2))
  val ups = vals.toList.zip(up).foldLeft(Map.empty[Int,Int])((map,vl) => map + (vl._1 -> vl._2))
  val alldoms = variables.foldLeft((Int.MaxValue,Int.MinValue))((set,v) => (math.min(set._1,v.dom.min),math.max(set._2,v.dom.max)))
  reset();
  //TODO: reset() should only be called after the model is closed, in case it makes use of invariants!
  def reset(){
    //TODO: This reset does not respect the domains of the variables!
    var v = vals(0)
    var i = 0;
    for(v <- 0 until vals.length){
      var c = 0;
      while(c < low(v)){
        variables(i) := vals(v)
        c += 1
        i += 1
      }
    }
    for(v <- 0 until vals.length){
      var c = low(v)
      while(i < variables.length && c < up(v)){
        variables(i) := vals(v)
        c += 1
        i += 1
      }
    }
    //need to put some variables outside, can only happen if not closed 
    if(i < variables.length){
      if(closed){
        throw new Exception("Closed GCC cannot be satisfied")
      }
      //TODO I am not happy with this code.
      while(i < variables.length){
        var found = false
        var v = variables(i).minVal
        while(!found){
          if(vals.forall(_!=v)){
            variables(i) := v
            i += 1
            found = true
          }
        }
      }
    }
  }
  def getSwapMove(idx1:Int,idx2:Int): Move = {
    //Swap will always respect the constraint is it was already satisfied
    new SwapMove(variables(idx1),variables(idx2),objective.swapVal(variables(idx1),variables(idx2)))
  }
  def getAssignMove(idx1:Int,v:Int): Move = {
    val cur = variables(idx1).value
    val cnt = counts.get(cur) match {case None => 1 case Some(x) => x.value}
    val lb = lows.getOrElse(cur,0)
    if(cnt <= lb) return new NoMove(Int.MaxValue);//using <= to protect from potential errors?
    val cnt2 = counts.get(cur) match {case None => 0 case Some(x) => x.value}
    val ub = ups.getOrElse(cur,1)
    if(cnt2 >= ub) return new NoMove(Int.MaxValue);//using >= to protect from potential errors?
    return new AssignMove(variables(idx1),v,objective.assignVal(variables(idx1), v));
  }
  
  def randomMove(it:Int):Move = {
    //TODO: respect the domains
    return getSwapMove(RandomGenerator.nextInt(variables.length),RandomGenerator.nextInt(variables.length))
  }
  
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    val idx = selectMax(rng2, (i: Int) => variableViolation(i).value);
    getBest(List(idx),rng2)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    getBest(rng2,rng2)
  }
  def getBest(rng1:Iterable[Int],rng2:Iterable[Int]): Move = {
    val bestSwap = selectMin2(rng1, rng2, (idx:Int,next:Int) => getSwapMove(idx,next).value,(idx:Int,v:Int) => variables(idx).dom.contains(variables(v).value) && variables(v).dom.contains(variables(idx).value) )
    val swap = bestSwap match { case (i1,i2) => getSwapMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    val bestMove = if(!closed){
      selectMin2(rng1,alldoms._1 to alldoms._2,(idx:Int,v:Int) => getAssignMove(idx,v).value,(idx:Int,v:Int) => variables(idx).dom.contains(v))
    }else{
      selectMin2(rng1,vals,(idx:Int,v:Int) => getAssignMove(idx,v).value,(idx:Int,v:Int) => variables(idx).dom.contains(v))
    }
    val move = bestMove match {case (i1,i2) => getAssignMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    if(swap.value < move.value) swap else move
  }
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}
//TODO: Take into account fixed variables!
class ThreeOpt(variables: Array[CBLSIntVarDom], objective: CBLSObjective, cs: ConstraintSystem, val offset: Int) extends Neighbourhood(variables){
  
  val variableViolation: Array[CBLSIntVar] = variables.map(cs.violation(_)).toArray
  val rng = offset until (offset+variables.length)
  reset();
  
  def reset(){
    //TODO: Add some randomization
    for(i <- rng){
      vars(i) := (i)%variables.length+offset 
    }
    //println(variables.map(v => v.value).mkString(","))
  }
  def vars(idx:Int): CBLSIntVarDom = {
    variables(idx-offset)
  }
  def randomMove(it:Int):Move = {
    val idx = rng(RandomGenerator.nextInt(variables.length))
    val next = rng(RandomGenerator.nextInt(variables.length))
    getMove(idx,next)
  }
  
  def getMove(idx: Int, nextnext: Int):Move = {
    if(idx==nextnext)return new NoMove(Int.MaxValue)//would break the chain
    val next = vars(idx).value
    if(next==nextnext)return new NoMove(Int.MaxValue)//would break the chain
    val k = vars(next).value
    val last = vars(nextnext).value
    val list = List((vars(idx),k),(vars(next),last),(vars(nextnext),next))
    val obj = objective.assignVal(list)
    return new AssignsMove(list,obj);
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val idx = selectMax(rng, (i: Int) => variableViolation(i-offset).value, (i: Int) => nonTabu.contains(vars(i)));
    val next = selectMin(rng.toList.filter(i => nonTabu.contains(vars(i))))(next =>getMove(idx,next).value)
   // println(idx +  " "+ getMove(idx,vars(idx).value))
    getMove(idx,next)
    
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    
    //println(variables.map(v => v.value).mkString(","))
    //this one removes a node and reinsert it somewhere else
    if(nonTabu.isEmpty)println("%EMPTY NON TABU")
    val rng2 = rng.toList.filter(i => nonTabu.contains(vars(i)));
    val res = selectMin2(rng2, rng2, (idx:Int,next:Int) => getMove(idx,next).value)
    //println(res +  " "+ (getMove _).tupled(res))//)._1,res._2))
    res match {
      case (idx,next) => getMove(idx,next)
      case _ => new NoMove(Int.MaxValue) //res is null when the NON TABU list is empty
    }
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}

class ThreeOptSub(variables: Array[CBLSIntVarDom], objective: CBLSObjective, cs: ConstraintSystem, offset: Int) extends ThreeOpt(variables, objective, cs, offset){
  //needed to add the allloop variable to be able to reinsert into the chain when the chain is only 1 element long.
  var allloop = false;
  override def reset(){
    super.reset();
    allloop = false;
  }
  override def getMove(idx: Int, nextnext: Int):Move = {
    val next = vars(idx).value
    val k = vars(next).value
    val last = vars(nextnext).value
    
    if(idx==next&&last==nextnext&& !allloop){
      //println("oops")
      return new NoMove(Int.MaxValue)//cannot join two selfloops unless there are only selfloops
    } 
    if(idx==nextnext&&idx==next){
      return new NoMove(Int.MaxValue)//cannot insert a selfloop into itself
    }
    //println("yep")
    if(idx==next&&idx!=nextnext){//idx is not in the chain, this should be an inclusion of idx after nextnext
      
      val list = List((vars(idx),last),(vars(nextnext),idx))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => allloop = false);
    }
    if(last==nextnext&&idx!=nextnext){//nextnext is not in the chain, this should be an inclusion of nextnext after idx
      val list = List((vars(idx),nextnext),(vars(nextnext),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => allloop = false);
    }
    if(idx==nextnext&&idx!=next){//assumes this is a removal of next
      val list = List((vars(idx),k),(vars(next),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => if(idx==k){
       // println(allloop)
        allloop = true
        //println(variables.map(v => v.value).mkString(","))
      });
    }
    if(next==nextnext&&idx!=next){//assumes this is a removal of next
      val list = List((vars(idx),k),(vars(next),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => if(idx==k){
        //println(allloop)
        allloop = true
        //println(variables.map(v => v.value).mkString(","))
      });
    }
    val list = List((vars(idx),k),(vars(next),last),(vars(nextnext),next))
    return new AssignsMove(list,objective.assignVal(list));
  }
}


class MaxViolating(searchVariables: Array[CBLSIntVarDom], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables) {
  
  val indexRange = 0 until searchVariables.length;
  val variableViolation: Array[CBLSIntVar] = searchVariables.map(constraintSystem.violation(_)).toArray
  
  
  def reset() = {
    for (v: CBLSIntVar <- searchVariables)
      v.setValue(v.domain(RandomGenerator.nextInt(v.domain.length)))
  }
  def randomMove(it: Int): Move = {
    val bestIndex = indexRange(RandomGenerator.nextInt(indexRange.length))
    val bestValue = searchVariables(bestIndex).getRandValue()
    val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); 
    return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    //TODO: Only takes into account the violation!
    val bestIndex = selectMax(indexRange, (i: Int) => variableViolation(i).value, (i: Int) => nonTabu.contains(searchVariables(i)));
    val bestValue = selectMin(searchVariables(bestIndex).getDomain())((i: Int) => objective.assignVal(searchVariables(bestIndex), i), _ != searchVariables(bestIndex).value)
    val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); 
    return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    var minSoFar = Int.MaxValue;
    var bestPair = List.empty[(Int, Int)]
    for (violVar <- indexRange if nonTabu.contains(searchVariables(violVar)) && searchVariables(violVar).domainSize < 10000000) {
      for ((valueInDomain: Int) <- searchVariables(violVar).getDomain()) {
        if (searchVariables(violVar).value != valueInDomain) {
          val viol = objective.assignVal(searchVariables(violVar), valueInDomain)
          if (viol < minSoFar || bestPair.isEmpty) {
            bestPair = List((violVar, valueInDomain))
            minSoFar = viol
          } else if (!bestPair.isEmpty && viol == minSoFar) {
            bestPair = (violVar, valueInDomain) :: bestPair
          }
        }
      }
    }
    if (!bestPair.isEmpty) {
      val (selectedVar, selectedVal) = bestPair.apply(RandomGenerator.nextInt(bestPair.length))
      val bestIndex = selectedVar
      val bestValue = selectedVal
      val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); ;
      return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
    } else {
      return new NoMove(Int.MaxValue);
    }
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}


//This neighborhood assumes that all variables have the same domain
class MaxViolatingSwap(searchVariables: Array[CBLSIntVarDom], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables) {
  val indexRange = 0 until searchVariables.length;
  val variableViolation: Array[CBLSIntVar] = searchVariables.map(constraintSystem.violation(_)).toArray
  
  def reset() = {

  }
  def randomMove(it: Int): Move = {
    val bestIndex1 = indexRange(RandomGenerator.nextInt(indexRange.length))
    val bestIndex2 = indexRange(RandomGenerator.nextInt(indexRange.length))
    val minObjective = objective.swapVal(searchVariables(bestIndex1), searchVariables(bestIndex2))
    return new SwapMove(searchVariables(bestIndex1), searchVariables(bestIndex2),minObjective);
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    //TODO: Only takes into account the violation for the first
    val bestIndex1 = selectMax(indexRange, (i: Int) => variableViolation(i).value, (i: Int) => nonTabu.contains(searchVariables(i)));
    val bestIndex2 = selectMin(indexRange)((i) => objective.swapVal(searchVariables(bestIndex1), searchVariables(i)), (i: Int) => searchVariables(i).value != searchVariables(bestIndex1).value && i != bestIndex1);
    if (!(nonTabu.contains(searchVariables(bestIndex1))) ||
      bestIndex1 == bestIndex2 || searchVariables(bestIndex2).value == searchVariables(bestIndex1).value) {
      return new NoMove(Int.MaxValue);
    } else {
      val minObjective = objective.swapVal(searchVariables(bestIndex1), searchVariables(bestIndex2))
      return new SwapMove(searchVariables(bestIndex1), searchVariables(bestIndex2),minObjective);
    }
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    getMinObjective(it, nonTabu);
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}



//This neighborhood is not totally "holes in the domain"-proof!
class AllDifferentEqDom(searchVariables: Array[CBLSIntVarDom], implicitConstants: List[CBLSIntVar], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables.filterNot((x) => implicitConstants.contains(x))) {
  /**/
  var swapMove: Boolean = true;//TODO: put this variable in the functions
  
  val constants: Array[CBLSIntVarDom] = searchVariables.filter((x) => implicitConstants.contains(x))
  val variables: Array[CBLSIntVarDom] = searchVariables.filterNot((x) => implicitConstants.contains(x))
  val indexRange: Range =0 until variables.length;
  val variableViolation: Array[CBLSIntVar] = variables.map(constraintSystem.violation(_)).toArray
  var freeValues: Set[Int] = Set.empty[Int]
  reset();
  
  def reset() = {
    freeValues = Set.empty[Int]
    for (i <- variables(0).minVal to variables(0).maxVal) {//assumes all variables have the same domain
      freeValues += i;
    }
    for (c <- constants) {
      freeValues -= c.value;
    }
    for (v <- variables) {
      val value = (if (RandomGenerator.nextBoolean()) { freeValues.head } else { freeValues.last });
      v.setValue(value);
      freeValues -= value;
    }
  }
  
  def randomMove(it: Int): Move = {
    if (freeValues.size > 0) {
      swapMove = RandomGenerator.nextBoolean()
    }
    if (swapMove) {
      val selectedIndex1 = indexRange(RandomGenerator.nextInt(indexRange.length))
      val selectedIndex2 = indexRange(RandomGenerator.nextInt(indexRange.length))
      val minObjective = objective.swapVal(searchVariables(selectedIndex1), searchVariables(selectedIndex2))
      return new SwapMove(searchVariables(selectedIndex1), searchVariables(selectedIndex2),minObjective);
    } else {
      val selectedIndex1 = indexRange(RandomGenerator.nextInt(indexRange.length))
      val selectedValue = variables(selectedIndex1).getRandValue()
      val minObjective = objective.assignVal(searchVariables(selectedIndex1), selectedValue)
      return new AssignMove(searchVariables(selectedIndex1), selectedValue,minObjective);
    }
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    //Change the max violating nontabu variable
    //TODO: Only takes into account the violation for the first selection
    val selectedIndex1 = selectMax(indexRange, (i: Int) => variableViolation(i).value, (i: Int) => nonTabu.contains(variables(i)));
    //Find the best swap move
    val selectedIndex2 = selectMin(indexRange)((i: Int) => objective.swapVal(variables(selectedIndex1), variables(i)), (i) => i != selectedIndex1)
    //Kepp the resulting objective
    val swapObjective = objective.swapVal(variables(selectedIndex1), variables(selectedIndex2))
    var minObjective = swapObjective;
    var selectedValue = 0;
    if (freeValues.size > 0) {
      //Find the best reassign move from the free values
      val selectedVIndex = selectedIndex1;
      selectedValue = selectMin(freeValues)((i: Int) => objective.assignVal(variables(selectedVIndex), i))
      swapMove = objective.assignVal(variables(selectedVIndex), selectedValue) > swapObjective;
      minObjective = Math.min(objective.assignVal(variables(selectedVIndex), selectedValue), swapObjective)
      
    }
    if(swapMove){
      return new SwapMove(searchVariables(selectedIndex1), searchVariables(selectedIndex2),minObjective);
    }else{ 
      return new AssignMove(searchVariables(selectedIndex1), selectedValue,minObjective);
    }
  }

  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]/*, minSoFar: Int*/): Move = {
    var selectedIndex1: Int = 0;
    var selectedIndex2: Int = 0;
  
    var selectedVIndex: Int = 0;
    var selectedValue: Int = 0;
    //Find the best swap move
    var swapObjective = 0;
    selectMin(indexRange, indexRange)((i: Int, j: Int) => objective.swapVal(variables(i), variables(j)),
      (i: Int, j: Int) => i < j &&
        (nonTabu.contains(variables(i)) || nonTabu.contains(variables(j) ) )) match {
        case (index1, index2) =>
          selectedIndex1 = index1;
          selectedIndex2 = index2;
          //Kepp the resulting objective
          swapObjective = objective.swapVal(variables(selectedIndex1), variables(selectedIndex2))
        case null =>
          swapObjective = Int.MaxValue
      }

    var minObjective = swapObjective;
    if (freeValues.size > 0) {
      selectMin(indexRange, freeValues)((i: Int, j: Int) => objective.assignVal(variables(i), j),
        (i: Int, j: Int) => nonTabu.contains(variables(i))) match {
          case (index, value) =>
            selectedVIndex = index;
            selectedValue = value;
            swapMove = swapObjective < objective.assignVal(variables(selectedVIndex), selectedValue)
            minObjective = Math.min(objective.assignVal(variables(selectedVIndex), selectedValue), swapObjective)
          case null =>
            //println("Failed to find move")
        }
    }
    //println(swapMove+" : " + minObjective + " " + freeValues.size)
    if(swapMove){
      return new SwapMove(searchVariables(selectedIndex1), searchVariables(selectedIndex2),minObjective);
    }else{ 
      return new BeforeMove(new AssignMove(searchVariables(selectedIndex1), selectedValue,minObjective),
                  () => {freeValues -= selectedValue; freeValues += variables(selectedVIndex).value});
    }
  }
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}