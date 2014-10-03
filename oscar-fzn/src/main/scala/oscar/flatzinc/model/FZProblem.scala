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
 * @author Leonard Debroux
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import scala.Array.canBuildFrom
import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.Range
import oscar.flatzinc.UnsatException

class FZProblem {

  var variables: List[Variable] = List.empty[Variable]
  
  var constraints: List[Constraint] = List.empty[Constraint]//TODO: might as well replace it by a list...
  var cstrsByName: MMap[String,List[Constraint]] = MMap.empty[String,List[Constraint]]
  val solution:FZSolution = new FZSolution();
  
  val search = new Search();
  
  def addVariable(id: String, dom: Domain, annotations: List[Annotation] = List.empty[Annotation]): Variable = {
   // println("% added var: "+id+ " with dom "+dom)
    val variable: Variable =
          new ConcreteVariable(id, dom,annotations)
//      if (dom.min == dom.max) {//why here?
//        new ConcreteConstant(id,dom.min,annotations)
//      } else {
//    //    map += id -> 
//    
//     //   map(id)
//      }
    variables = variable :: variables
    variable
  }
  
  def addConstraint(c: Constraint) {
    constraints = c :: constraints
    //the following code adds constraints by name
    val names = c.getClass().getName().split("\\.")
    var name = names(names.length-1)
    if(name=="reif"){
      val n2 = c.asInstanceOf[reif].c.getClass().getName().split("\\.")
      name += "_"+n2(n2.length-1)
    }
    cstrsByName(name) = List(c) ++ cstrsByName.getOrElse(name, List.empty[Constraint])
  }
  
  
  def satisfy() {
    search.obj = Objective.SATISFY
  }
  
  def minimize(obj: Variable) {
    search.obj = Objective.MINIMIZE
    search.variable = Some(obj)
  }
  
  def maximize(obj: Variable) {
    search.obj = Objective.MAXIMIZE
    search.variable = Some(obj)
  }
  
  def addSearch(s: Array[Variable],vrh: VariableHeuristic.Value,vh: ValueHeuristic.Value) {
    //println("search "+vrh+" "+vh+ " variables:"+s.mkString(","))
    search.heuristics =  search.heuristics :+ (s,vrh,vh)
  }
  
  def nSols(n: Int) {
    search.nSols = n
  }
}

abstract class Domain {
  def min: Int
  def max: Int
  def contains(v:Int): Boolean
  def size: Int
  def boundTo(v: Int) = min == v && max == v
  def geq(v:Int);
  def leq(v:Int);
  def inter(d:Domain):Unit = {
    if(d.isInstanceOf[DomainRange])inter(d.asInstanceOf[DomainRange])
    else inter(d.asInstanceOf[DomainSet])
  }
  def inter(d:DomainRange):Unit = {
    geq(d.min);
    leq(d.max);
  }
  def inter(d:DomainSet):Unit = {
    throw new UnsupportedOperationException("Inter of a Set")
  }
  def checkEmpty() = {
    if (min > max) throw new UnsatException("Empty Domain");
  }
}

case class DomainRange(var mi: Int, var ma: Int) extends Domain {
  def min = mi
  def max = ma
  def contains(v:Int): Boolean = mi <= v && ma >= v
  def size = ma-mi+1
  def geq(v:Int) = { mi = math.max(v,mi); checkEmpty() }
  def leq(v:Int) = { ma = math.min(v,ma); checkEmpty() }
  def toRange = mi to ma
  
}

case class DomainSet(var values: Set[Int]) extends Domain {
  def min = values.min
  def max = values.max
  def size = values.size
  def contains(v:Int): Boolean = values.contains(v)
  def geq(v:Int) = {values = values.filter(x => x>=v); checkEmpty() }
  def leq(v:Int) = {values = values.filter(x => x<=v); checkEmpty() }
  override def inter(d:DomainSet) = {values = values.intersect(d.values); checkEmpty() }
}


//TODO: differentiate between Int and Bool
//TODO: Add set variables
abstract class Variable(val id: String, val annotations: List[Annotation] = List.empty[Annotation]) {
  //val isIntroduced = annotations.foldLeft(false)((acc,x) => x.name=="var_is_introduced" || acc)//not interesting for us, as far as I know
  def isDefined: Boolean = {
    definingConstraint.isDefined//annotations.foldLeft(false)((acc,x) => x.name=="is_defined_var" || acc)
  }
  var definingConstraint: Option[Constraint] = Option.empty[Constraint]
  def min: Int
  def max: Int
  def is01: Boolean = min >= 0 && max <= 1
  def isTrue: Boolean = this.is01 && min == 1
  def isFalse: Boolean = this.is01 && max == 0 
  def isBound: Boolean = min == max
  override def toString = this.id
  var cstrs:List[Constraint] = List.empty[Constraint]
  def addConstraint(c:Constraint) = {
    cstrs = c :: cstrs
  }
  def removeConstraint(c:Constraint) = {
    //println("B"+cstrs)
    //println(c)
    cstrs = cstrs.filter(c != _)//might be made more efficient if cstrs was a set.
   //println("A"+cstrs)
  }
  def geq(v:Int);
  def leq(v:Int);
  def inter(d:Domain);
  def bind(v: Int) = {geq(v); leq(v);}
  def value:Int = {if(isBound) min else throw new Exception("Asking for the value of an unbound variable")}
}

case class ConcreteVariable(i: String,private var dom: Domain, anno: List[Annotation] = List.empty[Annotation]) extends Variable(i,anno) {
  def this(i: String, v: Int) = this(i,DomainRange(v,v),List.empty[Annotation]);
  def domain = dom
  def min = dom.min
  def max = dom.max
  def geq(v:Int) = dom.geq(v)
  def leq(v:Int) = dom.leq(v)
  def inter(d:Domain) = (dom,d) match {
    case (DomainRange(_,_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainSet(_)) => dom.inter(d)
    case (DomainRange(l,u),DomainSet(values)) => dom = DomainSet(values.filter(v => v>=l && v <= u)); dom.checkEmpty()
  }
}
/*
case class ConcreteConstant(i: String,val value:Int, anno: List[Annotation] = List.empty[Annotation]) extends Variable(i,anno){
  def min = value;
  def max = value;
}
*/

//TODO: should go to the CP specific part
object VariableHeuristic extends Enumeration {
  val FIRST_FAIL = Value("first_fail")
  val INPUT_ORDER = Value("input_order")
  val ANTI_FIRST_FAIL = Value("anti_first_fail")
  val SMALLEST = Value("smallest")
  val LARGEST = Value("largest")
  val OCCURENCE = Value("occurence")
  val MOST_CONSTRAINED = Value("most_constrained")
  val MAX_REGRET = Value("max_regret") 
}

object ValueHeuristic extends Enumeration {
  val INDOMAIN_MIN = Value("indomain_min")
  val INDOMAIN_MAX = Value("indomain_max")
  val INDOMAIN_MIDDLE = Value("indomain_middle")
  val INDOMAIN_MEDIAN = Value("indomain_median")
  val INDOMAIN = Value("indomain")
  val INDOMAIN_RANDOM = Value("indomain_random")
  val INDOMAIN_SPLIT = Value("indomain_split")
  val INDOMAIN_REVERSE_SPLIT = Value("indomain_reverse_split")
  val INDOMAIN_INTERVAL = Value("indomain_interval")  
}

object Objective extends Enumeration {
  val MINIMIZE = Value("minimize")
  val MAXIMIZE = Value("maximize")
  val SATISFY = Value("satisfy")
}



class Search() {
  var nSols = 0
  var obj: Objective.Value = Objective.SATISFY
  var variable: Option[Variable] = None
  var heuristics: Vector[(Array[Variable],VariableHeuristic.Value,ValueHeuristic.Value)] = Vector.empty 
}
