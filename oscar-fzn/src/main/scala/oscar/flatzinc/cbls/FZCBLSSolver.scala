/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls

import scala.util.Random
import scala.collection.mutable.{ Map => MMap, Set => MSet }
import oscar.cbls.search._
import oscar.cbls.objective.{ Objective => CBLSObjective }
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.constraints.lib.global._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
//import oscar.flatzinc.parser.NewParser
import oscar.flatzinc.parser.Options
import oscar.flatzinc.model._
import oscar.flatzinc.model.Constraint
import oscar.flatzinc.model.Variable
import oscar.flatzinc.cbls.support._
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.flatzinc.transfo.FZModelTransfo
import java.io.PrintWriter
import oscar.flatzinc.parser.FZParser


//TODO: Move this class somewhere else...
//TODO: Add several levels of logging
//TODO: Add the possibility to print to file or something else
class Log(opts:Options){
  def apply(s:String) = {
    if (opts.verbose) Console.err.println("% "+s)
  }
}

class FZCBLSObjective(opt: Objective.Value, val objectiveVar: CBLSIntVarDom,c:ConstraintSystem){
  val violationWeight = CBLSIntVar(c._model, 0 to Int.MaxValue , 1, "violation_weight")
  val objectiveWeight = CBLSIntVar(c._model, 0 to Int.MaxValue , 1, "objective_weight")
  val violation = c.violation;
  val objective: CBLSObjective = new CBLSObjective(
      opt match {
        case Objective.SATISFY => c.violation
        case Objective.MAXIMIZE => Minus(Prod2(c.violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
        case Objective.MINIMIZE => Sum2(Prod2(c.violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
      })
  def apply() = objective
  def getObjectiveValue(): Int = {
    return objectiveVar.value;
  }
  def increaseViolationWeight(minViolationSinceBest: Int){
    if (objectiveWeight.value > 1) {
      correctWeights(objectiveWeight.value / 2,violationWeight.value)
    } else {
      correctWeights(objectiveWeight.value,(violationWeight.value + Math.max(10, Math.abs(minViolationSinceBest / 2))).toInt)
    }
  }
  def increaseObjectiveWeight(minObjectiveSinceBest: Int){
    if (violationWeight.value > 1) {
      correctWeights(objectiveWeight.value,violationWeight.value / 2)
    } else {
      correctWeights((objectiveWeight.value + Math.max(10, Math.abs(minObjectiveSinceBest / 2))).toInt,violationWeight.value)
    }
  }
  def correctWeights(newObjW: Int,newVioW: Int){
    val objWub = Int.MaxValue/objectiveVar.maxVal/2
    val vioWub = Int.MaxValue/violation.maxVal/2 
    val minWeight = math.min(newObjW, newVioW)
    objectiveWeight := math.min(newObjW/minWeight,objWub)
    violationWeight := math.min(newVioW/ minWeight,vioWub) 
  }
}


class FZCBLSModel(val model: FZProblem, val c: ConstraintSystem, val m: Store, val log:Log, val getWatch: () => Long) {
  val cblsIntMap: MMap[String, CBLSIntVarDom] = MMap.empty[String, CBLSIntVarDom]
  var vars: List[CBLSIntVarDom] = createVariables();
  val objective = new FZCBLSObjective(model.search.obj,model.search.variable.map(getCBLSVar(_)).getOrElse(null) ,c)
  var neighbourhoods: List[Neighbourhood] = List.empty[Neighbourhood];
  
  def addNeighbourhood(n: Neighbourhood,removeVars: Boolean  = true){
    neighbourhoods = n :: neighbourhoods
    if(removeVars){
      vars = vars.filterNot(n.getVariables().contains(_))
    }
  }
  def addDefaultNeighbourhouds(){
    if (vars.length > 0) {
      addNeighbourhood(new MaxViolating(vars.toArray, objective(), c),false)
      val boolVars = vars.filter((v: CBLSIntVar) => v.minVal == 0 && v.maxVal == 1)
      if (boolVars.length > 0)
        addNeighbourhood(new MaxViolatingSwap(boolVars.toArray, objective(), c),false) 
    }
  }
  def createVariables() = {
    var searchVariables: List[CBLSIntVarDom] = List.empty[CBLSIntVarDom];
    for (parsedVariable <- model.variables) {
      parsedVariable match {
        case ConcreteVariable(id, dom, annotation) =>
          val initialValue = (dom match {
            case DomainRange(min, max) =>
              val range = (min to max);
//              println(range.length+ "\t"+range+"\t"+varName+ "\t"+ parsedVariable)
              range(Random.nextInt(range.length))
            case DomainSet(values) =>
              val v = values.toArray;
              v(Random.nextInt(v.length))
          });
          val cblsVariable = CBLSIntVarDom(m, dom, initialValue, id);
          //TODO: handle constant variables here.
          cblsIntMap += id -> cblsVariable;
          if (!parsedVariable.isDefined) {
            searchVariables = cblsVariable :: searchVariables;
          }
        case _ => ()//TODO: DO something for the concrete constants?
      }
    }
    searchVariables;
  }
    implicit def getCBLSVar(v: Variable) = {
      v match {
        case ConcreteConstant(_, value, _) =>
          //All constants need to have a store, otherwise they won't have a UniqueID (from PropagationElement) and constraints will start throwing exceptions
        cblsIntMap.get(value + "") match {
          case None =>
            val c = CBLSIntConstDom(value, m);
            cblsIntMap += value + "" -> c;
            c;
          case Some(c) => c;
        }
    
      case ConcreteVariable(id, _, _) =>
        cblsIntMap.get(id).get;
      }
    }
  def handleSolution() = {
    println("% time from start: "+getWatch())
    model.solution.handleSolution(
      (s: String) => cblsIntMap.get(s) match {
        case Some(intVar) =>
          intVar.value + "";
        case _ => throw new Exception("Unhappy")
      });
  }
  def getSolution():String = {
    model.solution.getSolution(
      (s: String) => cblsIntMap.get(s) match {
        case Some(intVar) =>
          intVar.value + "";
        case _ => throw new Exception("Unhappy")
      });
  }
}
class FZCBLSSolver extends SearchEngine with StopWatch {
  
  def solve(opts: Options): List[(Long, Int, Int, String)] = {
    startWatch()
    val log = new Log(opts);
    log("start")
    val m: Store = new Store(false, None, false)//setting the last Boolean to true would avoid calling the SCC algorithm but we have to make sure that there are no SCCs in the Graph. Is it the case in the way we build it?
    
    val model = FZParser.readFlatZincModelFromFile(opts.fileName).problem;
    model.cstrsByName.map{ case (n:String,l:List[Constraint]) => l.length +"\t"+n}.toList.sorted.foreach(log(_))
    log("Parsed. Parsing took "+getWatch+" ms")
    
    FZModelTransfo.propagateDomainBounds(model);
    log("Reduced Domains")
    if(!opts.is("no-find-inv")){
      FZModelTransfo.findInvariants(model,log);
      log("Found Invariants")
    }else{
      log("Did not search for new invariants")
    }
    if(opts.is("no-post-inv")){
      for(c <- model.constraints ){
        if(c.definedVar.isDefined)c.unsetDefinedVar(c.definedVar.get)
      }
    }
    
    
    
    
    
    // Model

    // constraint system
    val cs = ConstraintSystem(m)
    val cblsmodel = new FZCBLSModel(model,cs,m,log,() => getWatch)
    

    log("Created Model (Variables and Objective")
    
    //TODO: Most of those should be List instead of Array
    var constraints = model.constraints.toArray[Constraint];
    
    
    
    
    
    if(!opts.is("no-impl-cstr")){
      val implicitPoster = new FZCBLSImplicitConstraints(cblsmodel)
      val (implcstr,softcstr) = implicitPoster.findAndPostImplicit(constraints);
      constraints = softcstr
      log("Found "+cblsmodel.neighbourhoods .length+" Implicit Constraints")
      log(cblsmodel.neighbourhoods.mkString("\n"))
    }else{
      log("Did not try to find implicit constraints")
    }
    //println(constraints.size)
    
    val poster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(cs,cblsmodel.getCBLSVar);
    
    log("Possibly "+constraints.filter(_.definedVar.isDefined).length+" invariants.")
    var (invariants,removed) = FZModelTransfo.getSortedInvariants(constraints.filter(_.definedVar.isDefined))
    log("Sorted "+invariants.length+" Invariants")
    for (invariant <- invariants){
      //log("Posting as Invariant "+invariant)
      poster.add_invariant(invariant);
    }
    log("Posted "+invariants.length+" Invariants")
    
    val softConstraints = constraints.filterNot(_.definedVar.isDefined) /*++ removed*/;//removed is handled because the definedVar is undefined in getSortedInvariants.
    for (constraint <- softConstraints) {
     // log("Posting "+constraint)
      poster.add_constraint(constraint);
    }
    log("Posted "+softConstraints.length+" Soft Constraints")
    //println(implicitConstraints.length + " implicit constraints");
    
    //Do not want to search on such variables!
    cblsmodel.vars = cblsmodel.vars.filterNot(_.domainSize==1);
    cblsmodel.addDefaultNeighbourhouds()
    
    log("Using "+cblsmodel.vars.length+" Search Variables in default neighbourhoods")
    log("Created all Neighborhoods")
    
    //Search
    val timeout = (if(opts.timeOut>0) {opts.timeOut} else 5 * 60) * 1000
    log("Timeout is set to "+timeout+" milliseconds"); 
    val search =  new Chain(
        new SimpleLocalSearch(cblsmodel,CBLSObjective(cs.violation)/* objective*/,timeout),
        //new FakeSearch(),
        model.search.obj match {
          case Objective.SATISFY => new NeighbourhoodSearchSAT(cblsmodel, timeout);
          case Objective.MAXIMIZE => new NeighbourhoodSearchOPT(cblsmodel, - model.search.variable.get.max, timeout);
          case Objective.MINIMIZE => new NeighbourhoodSearchOPT(cblsmodel, model.search.variable.get.min, timeout);
        });
    
    log("Search created")
    m.close();
    log("Model closed");
    if(opts.is("no-run")){
      log("Not running the search...")
      return null;
    }else{
      log("Starting Search at "+getWatchString)
      val solutions = search.run();
      log("Done at "+getWatchString)
      if(solutions.length<=1){
        println("% Did not find any solution.")
        println("% Final violation: "+cs.violation.value)
      }
      return solutions;
    }
  }

  
  
  

  
}