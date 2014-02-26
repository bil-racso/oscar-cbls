package oscar.cp.minizinc

import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPIntVar
import scala.collection.mutable.{Map => MMap}
import oscar.cp.core.CPBoolVar
import scala.Array.canBuildFrom
import scala.collection.mutable.{Map => MMap}
import FZType._
import oscar.cp.constraints.And

class FZProblem {
  
  

  var map: Map[String,Variable] = Map.empty[String,Variable]
  
  var constraints: Set[Constraint] = Set.empty[Constraint]

  def addVariable(id: String, dom: Domain): Variable = {
    val variable: Variable =
      if (dom.min == dom.max) {
        if (!map.contains(dom.min + "")) {
          map += dom.min + "" -> new ConcreteVariable(id, dom)
        }
        map(dom.min + "")
      } else {
        new ConcreteVariable(id, dom)
      }
    map += id -> variable
    variable
  }
  
  def addVariable(id: String, v: Boolean): Variable = {
    addVariable(id,if (v) 1 else 0)
  }
  
  def addVariable(v: Boolean): Variable = {
    if (v) addVariable("1",v)
    else addVariable("0",v)
  }
  
  def addVariable(v: Int): Variable = {
    addVariable(v.toString,v)
  }    
  
  def addVariable(id: String, v: Int): Variable = {
    addVariable(id,v,v)
  }    
  
  def addVariable(id: String, min: Int,max: Int): Variable = {
    addVariable(id,DomainRange(min,max))
  }
  
  def addVariable(id: String, values: Set[Int]): Variable = {
    addVariable(id,DomainSet(values))
  }
  
  def addBoolVariable(id: String): Variable = {
    addVariable(id,DomainRange(0,1))
  } 
  
  def setVariable(id: String, x: Variable) {
    assert(id == x.id)
    map += id -> x
  }
  
  def addConstraint(c: Constraint) {
    constraints += c
  }
  
  
  def simplify() {
    constraints.foreach(_.simplify(this))
  }
  
  
  def instanciate(cp: CPSolver): Map[String,CPIntVar] = {
    var res = Map.empty[String,CPIntVar]
    val varMap = MMap[Variable,CPIntVar]()
    for ((s,v) <- map) {
      v.instanciate(cp, varMap)
      res += s -> varMap(v)
    }
    res
  }
  

}

abstract class Annotat

case class DefinesVar(id: String) extends Annotat

abstract class Domain {
  def min: Int
  def max: Int
  def size: Int
  def boundTo(v: Int) = min == v && max == v
}

case class DomainRange(val mi: Int, val ma: Int) extends Domain {
  def min = mi
  def max = ma
  def size = ma-mi+1
  
}

case class DomainSet(val values: Set[Int]) extends Domain {
  def min = values.min
  def max = values.max
  def size = values.size
}


abstract class Variable(val id: String, val isIntroduced: Boolean = false) {
  def instanciate(cp: CPSolver,varMap: MMap[Variable, CPIntVar])
  def min: Int
  def max: Int
  def is01: Boolean = min >= 0 && max <= 1
  def isTrue: Boolean = this.is01 && min == 1
  def isFalse: Boolean = this.is01 && max == 0
  
}

case class ConcreteVariable(i: String,dom: Domain) extends Variable(i) {
  def instanciate(cp: CPSolver,varMap: MMap[Variable, CPIntVar]) {
    if (varMap.contains(this)) return
    else {
      val variable: CPIntVar = dom match {
        case DomainRange(min, max) => CPIntVar(min to max)(cp)
        case DomainSet(v) => CPIntVar(v)(cp)
        case _ => throw new RuntimeException("unknown domain")
      }
      varMap += (this -> variable)
    }
  }
  
  def min = dom.min
  def max = dom.max
}

case class OffSetVariable(i: String,offset: Int,x: Variable) extends Variable(i) {
  def instanciate(cp: CPSolver,varMap: MMap[Variable, CPIntVar]) {
    if (varMap.contains(this)) return
    else {
      x.instanciate(cp, varMap)
      assert(varMap.contains(x))
      varMap += (this -> (varMap(x)+offset))
    }
  }
  def min = x.min + offset
  def max = x.max + offset
}

case class MinusVariable(i: String, x: Variable) extends Variable(i) {
  def instanciate(cp: CPSolver,varMap: MMap[Variable, CPIntVar]) {
    if (varMap.contains(this)) return
    else {
      x.instanciate(cp, varMap)
      assert(varMap.contains(x))
      varMap += (this -> (-varMap(x)))
    }
  }
  def min = -x.max
  def max = -x.min
}

abstract class Constraint(val annotations: Option[List[Annotat]] = None) {
  def simplify(p: FZProblem)
  
  def definesVar(x: Variable) = {
    annotations match {
      case None => false
      case Some(annotations) => {
        annotations.exists {
          _ match {
            case DefinesVar(id) => true
            case _ => false
          }
        }
      }
    }
  }
  
  def post(cp: CPSolver, varMap: Map[String,CPIntVar]) = {}
  
  

}




// ----------------------------------
case class array_bool_and(as: Array[Variable], r: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) {

  }
  
  override def post(cp: CPSolver, varMap: Map[String,CPIntVar]) {
    val x = as.map(y => new CPBoolVar(varMap(y.id)))
    val y = new CPBoolVar(varMap(r.id))
    cp.add(new And(x,y))
  }
  
  
}



case class bool_eq(x: Variable, y: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) {
    if (definesVar(y)) {
      p.map += y.id -> x
    } else {
      p.map += x.id -> y
    }
    p.constraints -= this
  }
  
  override def post(cp: CPSolver, varMap: Map[String,CPIntVar]) {
    throw new RuntimeException("should not be posted")
  }
}





//case class sum(args: Array[Variable]) extends Constraint

case class bool2int(b: Variable, x: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) {
    if (definesVar(x)) {
      p.map += x.id -> b
      p.constraints -= this
    } else {
       p.map += b.id -> x
      p.constraints -= this     
    }
  }
  
  override def post(cp: CPSolver, varMap: Map[String,CPIntVar]) {
    throw new RuntimeException("should not be posted")
  }  
  
}

case class int_eq(x: Variable, y: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) {
    if (definesVar(y)) {
      p.map += y.id -> x
    } else {
      p.map += x.id -> y
    }
    p.constraints -= this
  }
  
  override def post(cp: CPSolver, varMap: Map[String,CPIntVar]) {
    throw new RuntimeException("should not be posted")
  }
}



