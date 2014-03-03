package oscar.cp.minizinc

import scala.collection.mutable.{Map => MMap}
import scala.Array.canBuildFrom
import scala.collection.mutable.{Map => MMap}
import FZType._


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
    println("addcons")
    constraints += c
  }
  
  val search = new Search()
  
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
    println("search "+vrh+" "+vh+ " variables:"+s.mkString(","))
    search.heuristics =  search.heuristics :+ (s,vrh,vh)
  }
  
  def nSols(n: Int) {
    search.nSols = n
  }
  
  def simplify() {
    constraints.foreach(_.simplify(this))
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
  def min: Int
  def max: Int
  def is01: Boolean = min >= 0 && max <= 1
  def isTrue: Boolean = this.is01 && min == 1
  def isFalse: Boolean = this.is01 && max == 0 
}

case class ConcreteVariable(i: String,dom: Domain) extends Variable(i) {  
  def min = dom.min
  def max = dom.max
}

case class OffSetVariable(i: String,offset: Int,x: Variable) extends Variable(i) {
  def min = x.min + offset
  def max = x.max + offset
}

case class MinusVariable(i: String, x: Variable) extends Variable(i) {
  def min = -x.max
  def max = -x.min
}

abstract class Constraint(val annotations: Option[List[Annotat]] = None) {
  def simplify(p: FZProblem){}
  
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
}

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




// ----------------------------------
case class array_bool_and(as: Array[Variable], r: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) { }
}


case class array_bool_element(b: Variable, as: Array[Variable], c: Variable, ann: Option[List[Annotat]] = None) extends Constraint(ann) {
  override def simplify(p: FZProblem) { }
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
}

/*
         array_bool_and   

         array_bool_element   

         array_bool_or   


         array_int_element   
      

         array_var_bool_element   
      
         array_var_int_element   
      
         bool2int   
       
         bool_and   
     
         bool_eq   
       
         bool_eq_reif   
        
         bool_le   
       
         bool_le_reif   

         bool_lt   
 
         bool_lt_reif   

         bool_not   

         bool_or   

         bool_xor   

         bool_lin_eq   

         bool_lin_le   


         int_abs   

         int_eq   

         int_eq_reif   
 
         int_le   
 
         int_le_reif   

         int_lt   

         int_lt_reif   

         int_max   

         int_min   
         int_ne   
         int_ne_reif   
         int_plus   
         int_times   
   
         int_lin_ne   

         int_lin_ne_reif   

         int_lin_eq   

         int_lin_eq_reif   

         int_lin_le   

         int_lin_le_reif   


         set_card   

         set_diff   

         set_eq   
   
         set_in
   

         oscar_alldiff   

         alldiff_0   
         all_disjoint   
         oscar_all_equal_int   

         oscar_among   
         oscar_at_least_int   
         oscar_at most_int   
         at_most1   
         oscar_bin_packing   

         oscar_bin_packing_capa   

         oscar_bin_packing_load   

         oscar_circuit   
 
         oscar_count_eq   
 
         oscar_count_geq   
 
         oscar_count_gt   

         oscar_count_leq   

         oscar_count_lt   
    
         oscar_count_neq   
      
         oscar_cumulative

         oscar_decreasing_int   

         oscar_diffn   
     
         oscar_disjoint   

         oscar_distribute   

         oscar_element_bool   
         oscar_element_int   
         exactly_int    //not used, done with among

         oscar_global_cardinality   


         oscar_global_cardinality_closed   

         oscar_global_cardinality_low_up   

         oscar_global_cardinality_low_up_closed   
 
         oscar_increasing_int   

         oscar_int_set_channel   

         oscar_inverse   
         oscar_inverse_set   
         lex_greater_int    //not used, done with lex_less
         lex_greatereq_int    //not used, done with lex_lesseq
         oscar_lex_less_int   
         oscar_lex2    //2D -> 1D done, need to parse the constraint

         oscar_link_set_to_booleans   

         oscar_maximum_int   

         oscar_member_int   

         oscar_minimum_int   

         oscar_nvalue   

         oscar_partition_set   

         oscar_range   

         oscar_regular    //2D -> 1D done

         oscar_roots   

         oscar_sliding_sum   

         oscar_sort   

         oscar_strict_lex2   

         oscar_subcircuit   
 
         oscar_sum_pred   

         oscar_table_int    //2D -> 1D done

         oscar_value_precede_int   
 
         oscar_value_precede_chain_int   

*/

