package oscar.cp.minizinc

import scala.util.parsing.combinator._
import FZType._
import oscar.cp.modeling.CPSolver
import oscar.cp.core._
import oscar.cp.modeling._
import java.io.FileReader
import scala.Equals
import oscar.cp.constraints.EqReifVar
import oscar.cp.constraints.GrEqVarReif
import oscar.cp.constraints.DiffReifVar
import oscar.cp.constraints.EqReif
import oscar.cp.constraints.GrEqCteReif
import oscar.cp.constraints.DiffReif
import oscar.cp.constraints.Abs
import oscar.cp.constraints.Automaton
import oscar.cp.constraints.Sum
import scala.util.continuations._
import oscar.cp.constraints.SetDiff
import java.sql.Time
import oscar.cp.constraints.WeightedSum
import scala.collection.mutable.HashMap
import java.util.Collection
import oscar.algo.search.Branching
import oscar.cp.constraints.MulCte
import scala.collection.mutable.{Map => MMap}

class NewParser extends JavaTokenParsers { // RegexParsers {

  val UNDEFINED_VARINT_RANGE_MAX = 10000000
  val UNDEFINED_VARINT_RANGE_MIN = -10000000
  val PARTICULAR_SOLUTIONS = true
  var sol_found = false

  var model: FZProblem = new FZProblem()
  var dict = MMap[String, (FZType, FZObject)]()

  var options: Options = null

  def myParseAll(opts: Options) = {
    options = opts
    parseAll(flatzinc_model, opts.file)
  }
  def myParseAll(input: String) = { parseAll(flatzinc_model, input) }
  def parseParam(input: String) = { parseAll(param_decl, input) }
  def parseVar(input: String) = { parseAll(var_decl, input) }

  def reset() {
    model = new FZProblem()
  }

  /**
   * flatzinc model parsing
   */
  def flatzinc_model: Parser[Any] = rep(pred_decl) ~ rep(param_decl) ~ rep(var_decl) ~ rep(constraint) // ~ solve_goal

  /**
   * predicate declaration parsing
   */
  def pred_decl: Parser[Any] = "predicate" ~ identifier ~ "(" ~ rep1sep(pred_param, ",") ~ ");" ^^ {
    case "predicate" ~ id ~ "(" ~ parList ~ ");" => //println("predicate " + id)
    case _ => //println("error in predicate")
  }

  /**
   * identifier parsing (predicates)
   */
  def identifier: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r

  def pred_param: Parser[Any] = pred_param_type ~ ":" ~ pred_ann_id
  def pred_param_type: Parser[Any] = par_pred_param_type | var_pred_param_type

  /**
   * parameters types parsing
   */
  def par_type: Parser[Any] = (
    "bool"
    | "float"
    | "int"
    | "set of int"
    | "array [" ~ index_set ~ "] of bool"
    | "array [" ~ index_set ~ "] of float"
    | "array [" ~ index_set ~ "] of int"
    | "array [" ~ index_set ~ "] of set of int")

  /**
   * predicate parameters types parsing
   */
  def par_pred_param_type: Parser[Any] = (
    par_type
    | float_const ~ ".." ~ float_const
    | int_const ~ ".." ~ int_const
    | "{" ~ rep1sep(int_const, ",") ~ "}"
    | "set of" ~ int_const ~ ".." ~ int_const
    | "set of" ~ "{" ~ rep1sep(int_const, ",") ~ "}"
    | "array [" ~ index_set ~ "] of" ~ float_const ~ ".." ~ float_const
    | "array [" ~ index_set ~ "] of" ~ int_const ~ ".." ~ int_const
    | "array [" ~ index_set ~ "] of" ~ "{" ~ rep1sep(int_const, ",") ~ "}"
    | "array [" ~ index_set ~ "] of" ~ "set of" ~ int_const ~ ".." ~ int_const
    | "array [" ~ index_set ~ "] of" ~ "set of" ~ "{" ~ rep1sep(int_const, ",") ~ "}")

  /**
   * variables types parsing
   */
  def var_type: Parser[Any] = (
    "var bool"
    | "var int"
    | "var" ~ int_const ~ ".." ~ int_const
    | "var" ~ "{" ~ rep1sep(int_const, ",") ~ "}"
    | "var set of" ~ int_const ~ ".." ~ int_const
    | "var set of" ~ "{" ~ rep1sep(int_const, ",") ~ "}"
    | "array [" ~ index_set ~ "] of var bool"
    | "array [" ~ index_set ~ "] of var float"
    | "array [" ~ index_set ~ "] of var" ~ float_const ~ ".." ~ float_const
    | "array [" ~ index_set ~ "] of var int"
    | "array [" ~ index_set ~ "] of var" ~ int_const ~ ".." ~ int_const
    | "array [" ~ index_set ~ "] of var" ~ "{" ~ rep1sep(int_const, ",") ~ "}"
    | "array [" ~ index_set ~ "] of var set of" ~ int_const ~ ".." ~ int_const
    | "array [" ~ index_set ~ "] of var set of" ~ "{" ~ rep1sep(int_const, ",") ~ "}")
  
  def var_pred_param_type: Parser[Any] = (
    var_type
    | "var set of int"
    | "array [" ~ index_set ~ "] of var set of int")

  /**
   * parses index_set, used in array declarations
   */
  def index_set: Parser[Any] = (
    "1.." ~ int_const ^^ {
      case "1.." ~ i => Range(1, i + 1, 1)
    }
    | "int" ^^ (_.toString()) // what about the fact that "int" is only allowed in predicates ?
    )

  /**
   * expression parsing
   */
  def expr: Parser[Any] = (
    bool_const
    | set_const //should be float -> int -> set, inverted for set to work
    | float_const
    | int_const
    | var_par_id ~ "[" ~ int_const ~ "]" ^^ {
      case id ~ "[" ~ i ~ "]" => {
        val (t,obj) = dict(id)
        t match {
          case FZType.P_ARRAY_BOOL => obj.asInstanceOf[ParamArrayBool].values(i-1)
          case FZType.P_ARRAY_INT =>  obj.asInstanceOf[ParamArrayInt].values(i-1)
          case FZType.V_ARRAY_BOOL =>  obj.asInstanceOf[VarArrayInt].variables(i-1)
          case FZType.V_ARRAY_INT =>  obj.asInstanceOf[VarArrayInt].variables(i-1)
        }
        //List(id, i)
      }
    }
    | var_par_id ^^ {
      case id => {
        if (dict.contains(id)) {
          val (t,obj) = dict(id)
          t match {
            case FZType.P_BOOL => obj.asInstanceOf[ParamBool].value
            case FZType.P_INT => obj.asInstanceOf[ParamInt].value
            case FZType.V_BOOL => obj.asInstanceOf[VarInt].variable
            case FZType.V_INT => obj.asInstanceOf[VarInt].variable
          }
        }
      }
    }
    | array_expr
    | annotation
    | "[A-Z_a-z][A-Z_a-z0-9_]*".r //"...string constant..." //???
    )

  /**
   * identifiers, used for constraints and annotations
   */
  def pred_ann_id: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r

  /**
   * identifiers, used for parameters and variables declarations
   */
  def var_par_id: Parser[String] = "-*[A-Za-z][A-Za-z0-9_]*".r

  /**
   * definition of the constants
   */
  def bool_const: Parser[Boolean] = (
    "true" ^^ (x => true)
    | "false" ^^ (x => false))
  
  def float_const: Parser[Float] = (
    int_const ~ "." ~ "[0-9][0-9]*".r ~ opt("[eE]".r ~ int_const) ^^ {
      case i1 ~ "." ~ i2 ~ exp => exp match {
        case Some(e ~ i3) => (i1 + "." + i2 + e + i3).toFloat
        case None => (i1 + "." + i2).toFloat
      }
    }
    | int_const ~ "[eE]".r ~ int_const ^^ {
      case i1 ~ e ~ i2 => (i1 + e + i2).toFloat
    })
    
  def int_const: Parser[Int] = "[+-]?[0-9][0-9]*".r ^^ (_.toInt) // "?" added in the regex, as it wasn't in the grammar

  def set_const: Parser[Any] = (
    int_const ~ ".." ~ int_const ^^ {
      case int1 ~ ".." ~ int2 => Range(int1, int2 + 1)
    }
    | "{" ~> repsep(int_const, ",") <~ "}" //| "{"~>rep1sep(int_const, ",")<~"}" // -> according to the grammar, but doesn't parse some models
    )

  def array_expr: Parser[List[Any]] = (
    //"[]" | not useful since repsep is used instead of rep1sep
    "[" ~> repsep(expr, ",") <~ "]")

  /**
   * Parameter declarations
   */
  def param_decl: Parser[Any] = par_type ~ ":" ~ var_par_id ~ "=" ~ expr ~ ";" ^^
    {
      case tp ~ ":" ~ id ~ "=" ~ e ~ ";" =>
        tp match {
          case "bool" => dict +=
            ((id, (FZType.P_BOOL,
              new ParamBool(e.toString.toBoolean, id))))
          case "int" => dict +=
            ((id, (FZType.P_INT,
              new ParamInt(e.toString.toInt, id))))
          case "float" => dict +=
            ((id, (FZType.P_FLOAT,
              new ParamFloat(e.toString.toFloat, id))))
          case "set of int" => dict +=
            ((id, (FZType.P_SET_INT,
              e match {
                case x: Range => new ParamSetOfInt(x.toSet[Int], true, id)
                case x: List[Int] => new ParamSetOfInt(x.toSet[Int], false, id)
                case _ => throw new Exception("Error in parsing of set of int")
              })))

          case "array [" ~ iset ~ "] of bool" => dict +=
            ((id, (FZType.P_ARRAY_BOOL,
              new ParamArrayBool(e.asInstanceOf[List[Boolean]].toArray, id))))
            println(e)
          case "array [" ~ iset ~ "] of float" => dict +=
            ((id, (FZType.P_ARRAY_FLOAT,
              new ParamArrayFloat(e,
                iset match {
                  case x: Range => iset
                  case _ => None
                }, id))))
          case "array [" ~ iset ~ "] of int" => dict +=
            ((id, (FZType.P_ARRAY_INT,
              new ParamArrayInt(e.asInstanceOf[List[Int]].toArray, id))))
          case "array [" ~ iset ~ "] of set of int" => dict +=
            ((id, (FZType.P_ARRAY_SET_INT,
              new ParamArraySetOfInt(e,
                iset match {
                  case x: Range => iset
                  case _ => None
                }, id))))
          case _ => println("error")
        }
      case _ => println("err")
    }

  def var_decl: Parser[Any] = var_type ~ ":" ~ var_par_id ~ annotations ~ opt("=" ~ expr) ~ ";" ^^ {
    case tp ~ ":" ~ id ~ ann ~ e ~ ";" =>
      var t: FZType = null
      tp match {
        case "var bool" =>
          createCPBoolVar(e, id, ann)

        case "array [" ~ iset ~ "] of var bool" =>
          createBoolVarArray(e, id, ann, getRangeLength(iset))

        case "var int" =>
          createIntVar(e, id, Set[Int](), ann)

        case "var" ~ i1 ~ ".." ~ i2 =>
          val s = Range(i1.toString.toInt, i2.toString.toInt + 1).toSet[Int]
          if (!s.isEmpty) {
            createIntVar(e, id, s, ann)
          } else {
            throw new Exception("A var int can not have an empty domain")
          }

        case "var" ~ "{" ~ intList ~ "}" =>
          // no need to check if s is empty as the grammar doesn't allow it
          val s = getSetFromList(intList)
          createIntVar(e, id, s, ann)

        case "array [" ~ iset ~ "] of var int" =>
          createIntVarArray(e, id, Set[Int](), ann, getRangeLength(iset))

        case "array [" ~ iset ~ "] of var" ~ i1 ~ ".." ~ i2 =>
          val s = Range(i1.toString.toInt, i2.toString.toInt + 1).toSet
          if (!s.isEmpty) {   
            createIntVarArray(e, id, s, ann, getRangeLength(iset))
          } else {
            throw new Exception("A var int can not have an empty domain")
          }

        case "array [" ~ iset ~ "] of var" ~ "{" ~ intList ~ "}" =>
          println("here3")
          val s = getSetFromList(intList)
        //createCPIntVarArray(e, id, s, ann, getRangeLength(iset))

        case _ => throw new Exception("Error in parsing of var")
      }
    case _ => throw new Exception("Error in parsing of var")
  }

  def createCPBoolVar(e: Any, id: String, ann: List[Annotation]) {
    e match {
      case Some("=" ~ assign) =>
        assign match {
          case x: Boolean => dict +=
            ((id, (FZType.V_BOOL,
              new VarInt(ann,model.addVariable(id,0,1),id))))
          case _ =>
            dict += ((id, (FZType.V_BOOL, new VarInt(ann, getIntVar(assign), id))))
        }
      case None =>
          val variable = new VarInt(ann,model.addBoolVariable(id),id)
          dict += id -> (FZType.V_BOOL,variable)
      case _ => throw new Exception("Error in var bool creation")
    }
  }

  def createIntVar(e: Any, id: String, s: Set[Int], ann: List[Annotation]) {
    e match {
      case Some("=" ~ assign) =>
        assign match {
          case x: Int =>
            if ((s contains x) || s.isEmpty) {
              dict +=
                ((id, (FZType.V_INT,
                  new VarInt(ann, model.addVariable(id,x),id))))
            } else {
              throw new Exception(x + " not in the domain of " + id)
            }
          case _ =>
              println("domain:"+s)
              dict +=
                ((id, (FZType.V_INT,
                  new VarInt(ann, model.addVariable(id,s),id))))            
        }
      case None =>
          val vari =   if (s.max - s.min + 1 == s.size) model.addVariable(id,s.min,s.max) else model.addVariable(id,s)
          val variable = new VarInt(ann,vari,id)
          println("new var witn dom:"+s)
          dict += id -> (FZType.V_INT,variable)
      case _ => throw new Exception("Error in var int creation")
    }
  }

  def createBoolVarArray(e: Any, id: String, ann: List[Annotation], l: Int) {
    e match {
      case Some("=" ~ assign) =>
        assign match {
          case x: List[Any] => {
            val vars = getBoolVarArray(x)
            val boolArray = new VarArrayInt(ann,vars.toArray, id)
            dict += (id -> (FZType.V_ARRAY_BOOL, boolArray))
          }
          case _ =>
            // fresh array of boolvar
            val array = getBoolVarArray(assign)
            dict +=
              ((id, (FZType.V_ARRAY_BOOL,
                new VarArrayInt(ann, array, id))))
        }
      case None => {
        // fresh array of boolvar
        val boolArray = new VarArrayInt(ann, Array.tabulate(l) { i => model.addBoolVariable(id + "[" + (i + 1) + "]") }, id)
        dict += (id -> (FZType.V_ARRAY_BOOL, boolArray))
        // also add each individual entries
        for (i <- 1 to boolArray.size) {
          dict += ((id + "[" + i + "]") -> (FZType.V_BOOL, new VarInt(ann, boolArray.variables(i - 1), id)))
        }
      }

    }
  }
  
  def getBoolVarArray(x: Any): Array[Variable] = {
    x match {
      case y: List[Any] =>
        y.map(getIntVar(_)).toArray
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.V_ARRAY_BOOL => fzo.asInstanceOf[VarArrayInt].variables
              
              case FZType.P_ARRAY_BOOL => {
                fzo.asInstanceOf[ParamArrayBool].values.map(v => model.addVariable(v))
              }
            }
          case None => throw new Exception("Var " + x + " does not exist")
        }
    }
  }  
  
  

  def createIntVarArray(e: Any, id: String, s: Set[Int], ann: List[Annotation], l: Int) {
    e match {
      case Some("=" ~ assign) =>  
        println("assign array of var")
        assign match {
          case x: List[Any] => {
            val vars = getIntVarArray(x)
            val intArray = new VarArrayInt(ann,vars.toArray, id)
            dict += (id -> (FZType.V_ARRAY_INT, intArray))       
          }
          case _ =>
            println("array of intvar")
        }        
      case None =>
        val intArray =
          if (s.isEmpty) new VarArrayInt(ann, Array.tabulate(l) { i => model.addVariable(id + "[" + (i + 1) + "]", UNDEFINED_VARINT_RANGE_MIN, UNDEFINED_VARINT_RANGE_MAX) }, id)
          else new VarArrayInt(ann, Array.tabulate(l) { i => model.addVariable(id + "[" + (i + 1) + "]", s) }, id)
        dict += (id -> (FZType.V_ARRAY_INT, intArray))
        // also add each individual entries
        for (i <- 1 to intArray.size) {
          dict += ((id + "[" + i + "]") -> (FZType.V_INT, new VarInt(ann, intArray.variables(i - 1), id)))
        }     
    
      case _ => throw new Exception("Error in var int array creation")
    }
  }
  
  def getIntVarArray(x: Any): Array[Variable] = {
    x match {
      case y: List[Any] =>
        y.map(getIntVar(_)).toArray
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.V_ARRAY_INT => fzo.asInstanceOf[VarArrayInt].variables
              case FZType.P_ARRAY_INT => {
                fzo.asInstanceOf[ParamArrayBool].values.map(v => model.addVariable(v))
              }
            }
          case None => throw new Exception("Var " + x + " does not exist")
        }
    }
  }   
  

  def getIntVar(x: Any): Variable = {
    x match {
      case x: List[Any] => getVarFromList(x)
      case x: String => getVarFromString(x)
      case x: Int => model.addVariable(x)
      case x: Boolean => model.addVariable(x)
    }
  }  
  
  def getVarFromList(x: List[Any]): Variable = {
    dict.get(x(0).toString) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_ARRAY_BOOL => {
            fzo.asInstanceOf[VarArrayInt].variables(x(1).toString.toInt - 1)
          }
          case FZType.V_ARRAY_INT => {
            fzo.asInstanceOf[VarArrayInt].variables(x(1).toString.toInt - 1)
          }          
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
  } 
  

  
   def getVarFromString(x: String): Variable = {
    dict.get(x) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_BOOL => {
            fzo.asInstanceOf[VarInt].variable
          }
          case FZType.P_BOOL => {
            val pBool = fzo.asInstanceOf[ParamBool]
            model.addVariable(pBool.name, pBool.value)
          }
          case FZType.V_INT => {
            fzo.asInstanceOf[VarInt].variable
          }
          case FZType.P_INT => {
            val pInt = fzo.asInstanceOf[ParamInt]
            model.addVariable(pInt.name, pInt.value)
          }          
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
  }
   
   
  def getSetOfInt(x: Any): Set[Int] = {
    x match {
      case y: Range => y.toSet[Int]
      case y: List[Int] => y.toSet[Int]
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.P_SET_INT => {
                fzo.asInstanceOf[ParamSetOfInt].value
              }
            }
          case None => throw new Exception("Param " + x + " does not exist")
        }
    }
  }
   
   
  def getSetFromList(intList: Any): Set[Int] = {
    intList match {
      case x: List[Int] =>
        x.toSet[Int]
    }
  }  
 
  def getRangeLength(x: Any): Int = {
    x match {
      case y: Range => y.length
      case _ => 0
    }
  }
  
  
  // ------------------------------------------------------------------
  
  /**
   * Constraints parsing
   */
  def constraint: Parser[Any] = "constraint" ~ pred_ann_id ~ "(" ~ rep1sep(expr, ",") ~ ")" ~ annotations ~ ";" ^^ {
    case "constraint" ~ cstr ~ "(" ~ varList ~ ")" ~ ann ~ ";" => cstr match {

      case "array_bool_and" =>
        val array = getBoolVarArray(varList(0))
        //array_bool_cstr(varList, ann, cstr)
        
      /*  
      case "array_bool_element" =>
        System.err.println(cstr + " not implemented")
      case "array_bool_or" =>
        array_bool_cstr(varList, ann, cstr)

      case "array_int_element" =>
        val b = getCPIntVar(varList(0))
        val as = getIntArray(varList(1))
        val c = getCPIntVar(varList(2))
        addCstr(element(as, b - 1, c), ann)

      case "array_var_bool_element" =>
        val b = getCPIntVar(varList(0))
        val as = getCPBoolVarArray(varList(1))
        val c = getCPBoolVar(varList(2))
        addCstr(elementVar(as, b - 1, c), ann)
      case "array_var_int_element" =>
        val b = getCPIntVar(varList(0))
        val as = getCPIntVarArray(varList(1))
        val c = getCPIntVar(varList(2))
        addCstr(elementVar(as, b - 1, c), ann)
      case "bool2int" =>
      //cp.add(getCPBoolVar(varList(0)) == getCPIntVar(varList(1)))      
      case "bool_and" =>
        bool_cstr(varList, ann, cstr)
      case "bool_eq" =>
        bool_cstr(varList, ann, cstr)
      case "bool_eq_reif" =>
        bool_cstr(varList, ann, cstr)
      case "bool_le" =>
        bool_cstr(varList, ann, cstr)
      case "bool_le_reif" =>
        bool_cstr(varList, ann, cstr)
      case "bool_lt" =>
        bool_cstr(varList, ann, cstr)
      case "bool_lt_reif" =>
        bool_cstr(varList, ann, cstr)
      case "bool_not" =>
        bool_cstr(varList, ann, cstr)
      case "bool_or" =>
        bool_cstr(varList, ann, cstr)
      case "bool_xor" =>
        bool_cstr(varList, ann, cstr)

      case "bool_lin_eq" =>
        int_lin_cstr(varList, ann, cstr)
      case "bool_lin_le" =>
        int_lin_cstr(varList, ann, cstr)

      case "int_abs" =>
        int_cstr(varList, ann, cstr)
      case "int_eq" =>
        int_cstr(varList, ann, cstr)
      case "int_eq_reif" =>
        int_reif(varList, ann, cstr)
      case "int_le" =>
        int_cstr(varList, ann, cstr)
      case "int_le_reif" =>
        int_reif(varList, ann, cstr)
      case "int_lt" =>
        int_cstr(varList, ann, cstr)
      case "int_lt_reif" =>
        int_reif(varList, ann, cstr)
      case "int_max" =>
        val CPArray = Array[CPIntVar](getCPIntVar(varList(0)), getCPIntVar(varList(1)))
        addCstr(maximum(CPArray, getCPIntVar(varList(2))), ann)
      case "int_min" =>
        val CPArray = Array[CPIntVar](getCPIntVar(varList(0)), getCPIntVar(varList(1)))
        addCstr(minimum(CPArray, getCPIntVar(varList(2))), ann)
      case "int_ne" =>
        int_cstr(varList, ann, cstr)
      case "int_ne_reif" =>
        int_reif(varList, ann, cstr)
      case "int_plus" =>
        int_cstr(varList, ann, cstr)
      case "int_times" =>
        int_cstr(varList, ann, cstr)

      case "int_lin_ne" =>
        int_lin_cstr(varList, ann, cstr)
      case "int_lin_ne_reif" =>
        int_lin_cstr(varList, ann, cstr)
      case "int_lin_eq" =>
        int_lin_cstr(varList, ann, cstr)
      case "int_lin_eq_reif" =>
        int_lin_cstr(varList, ann, cstr)
      case "int_lin_le" =>
        int_lin_cstr(varList, ann, cstr)
      case "int_lin_le_reif" =>
        int_lin_cstr(varList, ann, cstr)

      case "set_card" =>
        val s = getCPSetVar(varList(0))
        val i = getCPIntVar(varList(1))
        addCstr(s.card == i, ann)
      case "set_diff" =>
        set_cstr(varList, ann, cstr)
      case "set_eq" =>
        set_cstr(varList, ann, cstr)
      case "set_in" => {
        val x = getCPIntVar(varList(0))
        val s = getSetOfInt(varList(1))
        for (v <- x.min to x.max; if !s.contains(v)) {
          x.removeValue(v)
        }
      }
      // global constraints defined in minizinc/mznlib/
      case "oscar_alldiff" =>
        cp.add(allDifferent(getCPIntVarArray(varList(0))), Strong)
      case "alldiff_0" =>
      case "all_disjoint" =>
      case "oscar_all_equal_int" =>
        // to be tested
        val array = getCPIntVarArray(varList(0))
        for (i <- 0 to array.length - 2) {
          addCstr(array(i) == array(i + 1), ann)
        }
      case "oscar_among" =>
        // no need to create the vals, can get while adding constraint, what is better ?
        val n = getCPIntVar(varList(0))
        val x = getCPIntVarArray(varList(1))
        val s = getSetOfInt(varList(2))
        addCstr(among(n, x, s), ann)
      case "oscar_at_least_int" =>
        // no need to create the vals, can get while adding constraint, what is better ?
        val n = getInt(varList(0))
        val x = getCPIntVarArray(varList(1))
        val v = getInt(varList(2))
        addCstr(atLeast(n, x, v), ann)
      case "oscar_at most_int" =>
        // no need to create the vals, can get while adding constraint, what is better ?
        val n = getInt(varList(0))
        val x = getCPIntVarArray(varList(1))
        val v = getInt(varList(2))
        addCstr(atMost(n, x, v), ann)
      case "at_most1" =>
      case "oscar_bin_packing" =>
        bin_packing(varList, "def")
      case "oscar_bin_packing_capa" =>
        bin_packing(varList, "capa")
      case "oscar_bin_packing_load" =>
        bin_packing(varList, "load")
      case "oscar_circuit" =>
        addCstr(circuit(getCPIntVarArray(varList(0)).map(_ - 1)), Strong)
      case "oscar_count_eq" =>
        count_cstr(varList, ann, cstr)
      case "oscar_count_geq" =>
        count_cstr(varList, ann, cstr)
      case "oscar_count_gt" =>
        count_cstr(varList, ann, cstr)
      case "oscar_count_leq" =>
        count_cstr(varList, ann, cstr)
      case "oscar_count_lt" =>
        count_cstr(varList, ann, cstr)
      case "oscar_count_neq" =>
        count_cstr(varList, ann, cstr)
      case "oscar_cumulative" => {
        val s = getCPIntVarArray(varList(0))
        val d = getCPIntVarArray(varList(1))
        val r = getCPIntVarArray(varList(2))
        val e = s.zip(d).map { case (st, dur) => st + dur }
        val c = getCPIntVar(varList(3))
        addCstr(maxCumulativeResource(s, d, e, r, c))
      }
      case "oscar_decreasing_int" =>
        val array = getCPIntVarArray(varList(0))
        for (i <- 0 to array.length - 2) {
          addCstr(array(i) >= array(i + 1), ann)
        }
      case "oscar_diffn" =>
        val x = getCPIntVarArray(varList(0))
        val y = getCPIntVarArray(varList(1))
        val dx = getCPIntVarArray(varList(2))
        val dy = getCPIntVarArray(varList(3))
        val endx = Array.tabulate(x.size)(i => x(i) + dx(i))
        val endy = Array.tabulate(y.size)(i => y(i) + dy(i))
        val capay = maximum(endy) - minimum(y)
        val capax = maximum(endx) - minimum(x)

        for (i <- 0 until x.length; j <- i + 1 until x.length) {
          cp.add(
            ((x(i) + dx(i) <== x(j)) || (x(j) + dx(j) <== x(i))) ||
              ((y(i) + dy(i) <== y(j)) || (y(j) + dy(j) <== y(i))))
        }
        addCstr(maxCumulativeResource(x, dx, endx, dy, capay))
        addCstr(maxCumulativeResource(y, dy, endy, dx, capax))
      case "oscar_disjoint" =>
        addCstr(disjoint(getCPSetVar(varList(0)), getCPSetVar(varList(1))), ann)
      case "oscar_distribute" =>
        System.err.println(cstr + " not implemented")
      case "oscar_element_bool" =>
        System.err.println(cstr + " not implemented")
      //cp.add(elementVar(getCPIntVarArray(varList(1).toString), getCPIntVar(varList(0)), getCPIntVar(varList(2))))
      case "oscar_element_int" =>
        addCstr(elementVar(getCPIntVarArray(varList(1)), getCPIntVar(varList(0)), getCPIntVar(varList(2))), ann)
      case "exactly_int" => //not used, done with among

      case "oscar_global_cardinality" =>
        gcc_cstr(varList)

      case "oscar_global_cardinality_closed" =>
        System.err.println(cstr + " not implemented")
      case "oscar_global_cardinality_low_up" =>
        gcc_lbub_cstr(varList)
      case "oscar_global_cardinality_low_up_closed" =>
        System.err.println(cstr + " not implemented")
      case "oscar_increasing_int" =>
        val array = getCPIntVarArray(varList(0))
        for (i <- 0 to array.length - 2) {
          addCstr(array(i) <= array(i + 1), ann)
        }
      case "oscar_int_set_channel" =>
        System.err.println(cstr + " not implemented")
      case "oscar_inverse" =>
        val x = getCPIntVarArray(varList(0))
        val y = getCPIntVarArray(varList(1))
        addCstr(permutation(x.map(_-1), y.map(_-1)), ann)
      //System.err.println(cstr+" not implemented")
      case "oscar_inverse_set" =>
        System.err.println(cstr + " not implemented")
      case "lex_greater_int" => //not used, done with lex_less
        System.err.println(cstr + " not implemented")
      case "lex_greatereq_int" => //not used, done with lex_lesseq
        System.err.println(cstr + " not implemented")
      case "oscar_lex_less_int" =>
        val t1 = getCPIntVarArray(varList(0))
        val t2 = getCPIntVarArray(varList(1))
        addCstr(lexLeq(t1, t2), ann)
        diff_array_cstr(t1, t2)
      case "oscar_lex_lesseq_int" =>
        addCstr(lexLeq(getCPIntVarArray(varList(0)), getCPIntVarArray(varList(1))), ann)
      case "oscar_lex2" => //2D -> 1D done, need to parse the constraint
        lex2_cstr(varList, false)
      case "oscar_link_set_to_booleans" =>
        System.err.println(cstr + " not implemented")
      case "oscar_maximum_int" =>
        addCstr(maximum(getCPIntVarArray(varList(1)), getCPIntVar(varList(0))), ann)
      case "oscar_member_int" =>
        System.err.println(cstr + " not implemented")
      case "oscar_minimum_int" =>
        addCstr(minimum(getCPIntVarArray(varList(1)), getCPIntVar(varList(0))), ann)
      case "oscar_nvalue" =>
        System.err.println(cstr + " not implemented")
      case "oscar_partition_set" =>
        System.err.println(cstr + " not implemented")
      case "oscar_range" =>
        System.err.println(cstr + " not implemented")
      case "oscar_regular" => //2D -> 1D done
        regular_cstr(varList)
      case "oscar_roots" =>
        System.err.println(cstr + " not implemented")
      case "oscar_sliding_sum" =>
        System.err.println(cstr + " not implemented")
      case "oscar_sort" =>
        sort_cstr(varList)
      case "oscar_strict_lex2" =>
        lex2_cstr(varList, true)
      case "oscar_subcircuit" =>
        System.err.println(cstr + " not implemented")
      case "oscar_sum_pred" =>
        System.err.println(cstr + " not implemented")
      case "oscar_table_int" => //2D -> 1D done
        table_cstr(varList)
      case "oscar_value_precede_int" =>
        System.err.println(cstr + " not implemented")
      case "oscar_value_precede_chain_int" =>
        System.err.println(cstr + " not implemented")
        
        */
    }
  }  


/*



  def addCPIntVar(ann: List[Annotation], id: String, s: Set[Int]) {

      dict += ((id, (FZType.V_INT,
        new VarInt(ann,
          if (s.isEmpty) {
            model.addVariable(id,UNDEFINED_VARINT_RANGE_MIN, UNDEFINED_VARINT_RANGE_MAX)
          } else {
            model.addVariable(id,s)
          }, id))))
  }


  def addCPIntVar(ann: List[Annotation], id: String, cpvar: Variable) {
    dict += ((id, (FZType.V_INT, new VarInt(ann, cpvar, id))))
  }


  
  def addCPIntVarArray(ann: List[Annotation], id: String, s: Set[Int], l: Int) {
    dict +=
      ((id, (FZType.V_ARRAY_INT,
        new VarArrayInt(s, ann,
          if (s.isEmpty) {
            Array.fill(l) { CPIntVar(UNDEFINED_VARINT_RANGE_MIN, UNDEFINED_VARINT_RANGE_MAX)(cp) }
          } else {
            Array.fill(l) { CPIntVar(s)(cp) }
          }, id))))
  }

  def addCPIntVarArray(ann: List[Annotation], id: String, s: Set[Int], array: Array[Variable]) {
    dict +=
      ((id, (FZType.V_ARRAY_INT,
        new VarArrayInt(s, ann, array, id))))
  }




  def getBool(x: Any): Boolean = {
    x match {
      case y: Boolean => y
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.P_BOOL => {
                fzo.asInstanceOf[ParamBool].value
              }
            }
          case None => throw new Exception("Param " + x + " does not exist")
        }
    }
  }


  def getInt(x: Any): Int = {
    x match {
      case y: Int => y
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.P_INT => {
                fzo.asInstanceOf[ParamInt].value
              }
            }
          case None => throw new Exception("Param " + x + " does not exist")
        }
    }
  }


  def getBoolArray(x: Any): Array[Boolean] = {
    x match {
      case y: List[Any] => y.asInstanceOf[List[Boolean]].toArray
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.P_ARRAY_BOOL => {
                val list = fzo.asInstanceOf[ParamArrayBool].value.asInstanceOf[List[Boolean]]
                list.toArray
              }
            }
          case None => throw new Exception("Param " + x + " does not exist")
        }
    }
  }


  def getIntArray(x: Any): Array[Int] = {
    x match {
      case y: List[Any] => y.asInstanceOf[List[Int]].toArray
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.P_ARRAY_INT => {
                val list = fzo.asInstanceOf[ParamArrayInt].value.asInstanceOf[List[Int]]
                (list map (_.toInt)).toArray
              }
            }
          case None => throw new Exception("Param " + x + " does not exist")
        }
    }
  }






  def getCPIntVar(x: Any,id: String): Variable = {
    x match {
      case x: Int => model.addVariable(id,x,x)
      case x: List[Any] => getCPIntVarFromList(x)
      case x: String => getCPIntVarFromString(x)
    }
  }



  def getCPIntVarFromString(x: String): Variable = {
    dict.get(x) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_INT => {
            fzo.asInstanceOf[VarInt].cpvar
          }
          case FZType.V_BOOL => {
            //System.err.println("found it!!")
            fzo.asInstanceOf[VarInt].cpvar
          }
          case FZType.P_INT => {
            model.add()
            CPIntVar(fzo.asInstanceOf[ParamInt].value)(cp)
          }
          case _ => {
            throw new Exception("Var " + x + " does not match")
          }
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
  }






  def getCPIntVarFromList(x: List[Any]): Variable = {
    dict.get(x(0).toString) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_ARRAY_INT => {
            fzo.asInstanceOf[Variable].cpvar(x(1).toString.toInt - 1)
          }
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
  }





  def getCPIntVarArray(x: Any): Array[CPIntVar] = {
    x match {
      case y: List[Any] =>
        (y) map (getCPIntVar(_)) toArray
      case y: String =>
        dict.get(y) match {
          case Some((tp, fzo)) =>
            tp match {
              case FZType.V_ARRAY_INT => {
                fzo.asInstanceOf[VarArrayInt].cpvar
              }
              case FZType.P_ARRAY_INT => {
                val array = fzo.asInstanceOf[ParamArrayInt].value
                array match {
                  case x: List[Int] =>
                    (x) map (CPIntVar(_)(cp)) toArray
                }
              }
            }
          case None => throw new Exception("Var " + y + " does not exist")
        }
    }
  }


  def getCPArrayRangeSize(x: String): Int = {
    dict.get(x) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_ARRAY_INT => {
            fzo.asInstanceOf[VarArrayInt].value.size
          }
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
  }

  def getCPArrayOutputAnnotations(x: String): List[Range] = {
    var l = List[Range]()
    dict.get(x) match {
      case Some((tp, fzo)) =>
        tp match {
          case FZType.V_ARRAY_BOOL => {
            for (
              ann <- fzo.asInstanceOf[VarArrayBool].annotations if (ann.name == "output_array")
            ) {
              l = ann.args.asInstanceOf[List[List[Range]]](0)
            }
          }
          case FZType.V_ARRAY_INT => {
            for (
              ann <- fzo.asInstanceOf[VarArrayInt].annotations if (ann.name == "output_array")
            ) {
              l = ann.args.asInstanceOf[List[List[Range]]](0)
            }
          }
          case FZType.V_ARRAY_SET => {
            for (
              ann <- fzo.asInstanceOf[VarArraySet].annotations if (ann.name == "output_array")
            ) {
              l = ann.args.asInstanceOf[List[List[Range]]](0)
            }
          }
        }
      case None => throw new Exception("Var " + x + " does not exist")
    }
    l
  }



*/


  def annotations: Parser[List[Annotation]] =
    "::" ~> "seq_search" ~> "(" ~> "[" ~> repsep(annotation, ",") <~ "]" <~ ")" | rep("::" ~> annotation)
  // list of annotations : in flatzinc spec pg10
  def annotation: Parser[Annotation] = (
    pred_ann_id ~ "(" ~ rep1sep(expr, ",") ~ ")" ^^ {
      case ann ~ "(" ~ list ~ ")" => new Annotation(ann, list)
    }
    | pred_ann_id ^^ (new Annotation(_, null)))

}
