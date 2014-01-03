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
import oscar.cp.constraints.Or
import oscar.cp.constraints.Sum
import scala.util.continuations._
import oscar.cp.constraints.SetDiff
import java.sql.Time
import oscar.cp.constraints.WeightedSum
import scala.collection.mutable.HashMap
import oscar.cp.scheduling.CumulativeActivity
import java.util.Collection

class Parser extends JavaTokenParsers {// RegexParsers {
  
	val UNDEFINED_VARINT_RANGE_MAX = 10000000
	val UNDEFINED_VARINT_RANGE_MIN = -10000000
	val PARTICULAR_SOLUTIONS = true
	var sol_found = false
	
	var model : Minizinc_model = new Minizinc_model
	var cp = CPSolver()
	
	var options: Options = null
	var bool2Int: Map[String,String] = null
	//def myParseAll(input: String) = {parseAll(var_decl, input)}
	
	def myParseAll(opts: Options,b2i: Map[String,String]) = {
	  options = opts
	  bool2Int = b2i
	  parseAll(flatzinc_model, opts.file)
	}
	def myParseAll(input: String) = {parseAll(flatzinc_model, input)}
	//def myParseAll(input: String) = {parseAll(constraint, input)}
	def parseParam(input: String) = {parseAll(param_decl, input)}
	def parseVar(input: String) = {parseAll(var_decl, input)}
	
	def reset() {
	  model = new Minizinc_model
	  cp = CPSolver()
	}
	
	/**
	 * flatzinc model parsing
	 */
	def flatzinc_model : Parser[Any] = rep(pred_decl)~rep(param_decl)~rep(var_decl)~rep(constraint)~solve_goal

   /**
	* predicate declaration parsing
	*/			
	def pred_decl : Parser[Any] = "predicate"~identifier~"("~rep1sep(pred_param, ",")~");" ^^ {
	  case "predicate"~id~"("~parList~");" => //println("predicate " + id)
	  case _ => //println("error in predicate")
	}

	/**
	 * identifier parsing (predicates)
	 */
	def identifier : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	def pred_param : Parser[Any] = pred_param_type~":"~pred_ann_id 
	def pred_param_type : Parser[Any] = par_pred_param_type | var_pred_param_type	
	
	/**
	 * parameters types parsing
	 */
	def par_type : Parser[Any] = (
	    "bool"
		| "float"
		| "int"
		| "set of int"
		| "array ["~index_set~"] of bool"
		| "array ["~index_set~"] of float"
		| "array ["~index_set~"] of int"
		| "array ["~index_set~"] of set of int"
	)
	
	/**
	 * predicate parameters types parsing
	 */
	def par_pred_param_type : Parser[Any] = (
		par_type
		| float_const~".."~float_const
		| int_const~".."~int_const
		| "{"~rep1sep(int_const, ",")~"}"
		| "set of"~int_const~".."~int_const
		| "set of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~float_const~".."~float_const
		| "array ["~index_set~"] of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~"set of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	
	/**
	 * variables types parsing
	 */
	def var_type : Parser[Any] = (
	    "var bool"
		| "var float"
	    | "var"~float_const~".."~float_const
	    | "var int"
	    | "var"~int_const~".."~int_const
	    | "var"~"{"~rep1sep(int_const, ",")~"}"
	    | "var set of"~int_const~".."~int_const
	    | "var set of"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var bool"
	    | "array ["~index_set~"] of var float"
	    | "array ["~index_set~"] of var"~float_const~".."~float_const
	    | "array ["~index_set~"] of var int"
	    | "array ["~index_set~"] of var"~int_const~".."~int_const
	    | "array ["~index_set~"] of var"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var set of"~int_const~".."~int_const
	    | "array ["~index_set~"] of var set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_pred_param_type : Parser[Any] = (
		var_type
		| "var set of int"
		| "array ["~index_set~"] of var set of int"
	)
	
	/**
	 * parses index_set, used in array declarations
	 */
	def index_set : Parser[Any] = (
	    "1.."~int_const ^^ {
	      case "1.."~i => Range(1, i+1, 1)
	    }
	    | "int" ^^ (_.toString())// what about the fact that "int" is only allowed in predicates ?
	)
	
	/**
	 * expression parsing
	 */
	def expr : Parser[Any] = ( 
		bool_const
		| set_const //should be float -> int -> set, inverted for set to work
		| float_const
		| int_const
		| var_par_id~"["~int_const~"]" ^^ {
		  case id~"["~i~"]" => List(id, i)
		}
		| var_par_id
		| array_expr
		| annotation
		| "[A-Z_a-z][A-Z_a-z0-9_]*".r //"...string constant..." //???
	)
	
	/**
	 * identifiers, used for constraints and annotations
	 */
	def pred_ann_id : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	/**
	 * identifiers, used for parameters and variables declarations
	 */
	def var_par_id : Parser[String] = "-*[A-Za-z][A-Za-z0-9_]*".r
	
	/**
	 * definition of the constants
	 */
	def bool_const : Parser[Boolean] = (
	    "true" ^^ (x => true)
	    | "false" ^^ (x => false)
	)
	def float_const : Parser[Float] = (
	    int_const~"."~"[0-9][0-9]*".r~opt("[eE]".r~int_const) ^^ {
	      case i1~"."~i2~exp => exp match {
	        case Some(e~i3) => (i1+"."+i2+e+i3).toFloat
	        case None => (i1+"."+i2).toFloat
	      }
	    }
	    | int_const~"[eE]".r~int_const ^^ {
	      case i1~e~i2 => (i1+e+i2).toFloat
	    }
	)
	def int_const : Parser[Int] = "[+-]?[0-9][0-9]*".r ^^ (_.toInt) // "?" added in the regex, as it wasn't in the grammar
	
	def set_const : Parser[Any] = ( 
	    int_const~".."~int_const ^^ { 
	    	case int1~".."~int2 => Range(int1, int2+1) 
	    } 
	    | "{"~>repsep(int_const, ",")<~"}" 
	    //| "{"~>rep1sep(int_const, ",")<~"}" // -> according to the grammar, but doesn't parse some models
	)
	
	def array_expr : Parser[List[Any]] = (
	    //"[]" | not useful since repsep is used instead of rep1sep
	    "["~>repsep(expr, ",")<~"]"
	)
	
	/**
	 * Parameter declarations
	 */
	def param_decl : Parser[Any] = par_type~":"~var_par_id~"="~expr~";" ^^ 
	{
	  case tp~":"~id~"="~e~";" =>
	    tp match {
	      case "bool" => model.dict += 
	        ((id, (FZType.P_BOOL, 
	            new ParamBool(e.toString.toBoolean, id))))
	      case "int" => model.dict += 
	        ((id, (FZType.P_INT, 
	            new ParamInt(e.toString.toInt, id))))
	      case "float" => model.dict += 
	        ((id, (FZType.P_FLOAT,
	            new ParamFloat(e.toString.toFloat, id))))
	      case "set of int" => model.dict += 
	        ((id, (FZType.P_SET_INT,
	          e match {
	          	case x:Range => new ParamSetOfInt(x.toSet[Int], true, id)
	          	case x:List[Int] => new ParamSetOfInt(x.toSet[Int], false, id)
	          	case _ => throw new Exception("Error in parsing of set of int")
	          }
	        )))
	        
	      case "array ["~iset~"] of bool" => model.dict += 
	        ((id, (FZType.P_ARRAY_BOOL, 
	            new ParamArrayBool(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of float" => model.dict += 
	        ((id, (FZType.P_ARRAY_FLOAT, 
	            new ParamArrayFloat(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of int" => model.dict += 
	        ((id, (FZType.P_ARRAY_INT, 
	            new ParamArrayInt(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of set of int" => model.dict +=
	        ((id, (FZType.P_ARRAY_SET_INT, 
	            new ParamArraySetOfInt(e,
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	    }
	}
	
	/**
	 * Variables declarations
	 */
	def var_decl : Parser[Any] = var_type~":"~var_par_id~annotations~opt("="~expr)~";" ^^
	{ 
	  case tp~":"~id~ann~e~";" => 
	    var t: FZType = null
	    tp match {
	      case "var bool" => 
	        createCPVarBool(e, id, ann)
	        
	      case "array ["~iset~"] of var bool" =>
	        createCPVarBoolArray(e, id, ann, getRangeLength(iset))
	            	
	      case "var int" => 
	        createCPVarInt(e, id, Set[Int](), ann)
	        
	      case "var"~i1~".."~i2 => 
	        val s = Range(i1.toString.toInt, i2.toString.toInt+1).toSet[Int]
	        if(!s.isEmpty) {
	          createCPVarInt(e, id, s, ann)
	        } else {
	          throw new Exception("A var int can not have an empty domain")
	        }      
	        
	      case "var"~"{"~intList~"}" =>
	        // no need to check if s is empty as the grammar doesn't allow it
	        val s = getSetFromList(intList)
	        createCPVarInt(e, id, s, ann)
	        
	      case "var set of"~i1~".."~i2 => 
	        val s = Range(i1.toString.toInt, i2.toString.toInt+1).toSet[Int]
	        createCPVarSet(e, id, s, ann)
	        
	      case "var set of"~"{"~intList~"}" => 
	        val s = getSetFromList(intList)
	        createCPVarSet(e, id, s, ann)

	      case "array ["~iset~"] of var int" => 
	        createCPVarIntArray(e, id, Set[Int](), ann, getRangeLength(iset))
	                
	      case "array ["~iset~"] of var"~i1~".."~i2 => 
	        val s = Range(i1.toString.toInt, i2.toString.toInt+1).toSet
	        if(!s.isEmpty) {
	          createCPVarIntArray(e, id, s, ann, getRangeLength(iset))
	        } else {
	          throw new Exception("A var int can not have an empty domain")
	        } 
	            	
	      case "array ["~iset~"] of var"~"{"~intList~"}" => 
	        val s = getSetFromList(intList)
	        createCPVarIntArray(e, id, s, ann, getRangeLength(iset))
	                  
	      case "array ["~iset~"] of var set of"~i1~".."~i2 => 
	        val s = Range(i1.toString.toInt, i2.toString.toInt+1, 1).toSet[Int]
	        createCPVarSetArray(e, id, s, ann, getRangeLength(iset))
	            	
	      case "array ["~iset~"] of var set of"~"{"~intList~"}" => 
	        // grammar doesn't allow to create an empty set, should it be modified ? (cfr var_type)
	        val s = getSetFromList(intList)
	        createCPVarSetArray(e, id, s, ann, getRangeLength(iset))
	                  
	      case _ => throw new Exception("Error in parsing of var")
	    }
	}
	
	/**
	 * Creates a CPVarBool and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the Var if it is assigned in the model
	 * @param id : the name of the variable
	 * @param ann : the list of annotation for the variable
	 */
	def createCPVarBool(e: Any, id: String, ann: List[Annotation]) {
	  e match {
	      case Some("="~assign) =>
	        assign match {
	      	  case x:Boolean => model.dict +=
		        ((id, (FZType.V_BOOL,
		            new VarBool(ann, CPVarBool(x)(cp), id))))
		      case _ => 
		        addCPVarBool(ann, id, getCPVarBool(assign))
	      	}
	      case None => 
	        addCPVarBool(ann, id)
		  case _ => throw new Exception("Error in var bool creation")
	    }
	}
	/**
	 * Creates a CPVarInt and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the Var if it is assigned in the model
	 * @param id : the name of the variable
	 * @param s : a set, the initial domain of the variable 
	 * @param ann : the list of annotation for the variable
	 */
	def createCPVarInt(e: Any, id: String, s: Set[Int], ann: List[Annotation]) {
	  e match {
          case Some("="~assign) =>
            assign match {
	      	  case x:Int => 
	      	    if((s contains x) || s.isEmpty) {
	      	      model.dict += 
	      	        ((id, (FZType.V_INT, 
	      	            new VarInt(ann, CPVarInt(x)(cp), id))))
	      	    } else {
	      	      throw new Exception(x + " not in the domain of " + id)
	      	    }
	      	  case _ => 
	      	    val cpvar = getCPVarInt(assign)
	      	    if(!s.isEmpty) {
	      	      shrinkDom(s, cpvar)
	      	    }
	      	    addCPVarInt(ann, id, cpvar)
	      	}
          case None => 
            addCPVarInt(ann, id, s)
          case _ => throw new Exception("Error in var int creation")
        }
	}
	
	/**
	 * Creates a CPVarSet and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the Var if it is assigned in the model
	 * @param id : the name of the variable
	 * @param s : a set, the initial domain of the variable 
	 * @param ann : the list of annotation for the variable
	 */
	def createCPVarSet(e: Any, id: String, s: Set[Int], ann: List[Annotation]) {
	  e match {
          case Some("="~assign) =>
            assign match {
              case x:List[Int] if(x(0).isInstanceOf[Int]) => 
                if(x.toSet.subsetOf(s)) {
                  model.dict += 
		      		((id, (FZType.V_SET_INT, 
		      		    // it is an assignment, so x is a requiered set for the CPVarSet
		      			new VarSetInt(ann, CPVarSet(x.toSet, x.toSet)(cp), id)
		      		)))
                } else {
                  throw new Exception(x.toSet.toString + " not in the domain of " + id)
                }
              case _ => 
                val cpvar = getCPVarSet(assign)
	      	    shrinkDom(s, cpvar)
	      	    addCPVarSet(ann, id, cpvar)
            }
          case None =>
	        addCPVarSet(ann, id, s)
	      case _ => throw new Exception("Error in var set creation")
        }
	}
	
	/**
	 * Creates a array of CPVarBool and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the array of var if it is assigned in the model
	 * @param id : the name of the array
	 * @param ann : the list of annotation for the array of variables
	 */
	def createCPVarBoolArray(e: Any, id: String, ann: List[Annotation], l: Int) {
	  e match {
          case Some("="~assign) =>
            assign match {
              case x:List[Any] => {
                val boolArray = new VarArrayBool(ann, (x) map(getCPVarBool(_)) toArray,id)
                model.dict += (id -> (FZType.V_ARRAY_BOOL, boolArray))
		        /*
                // also add each individual entries
                for (i <- 1 to boolArray.cpvar.size) {
                  println("adding artificially:"+id+"["+i+"]")
                  model.dict += (id+"["+i+"]" -> (FZType.V_BOOL, new VarBool(ann,boolArray.cpvar(i-1),id)))
                }
                */
              }
		         
              case _ => 
	      	    val value = getCPVarBoolArray(assign)
	      	    addCPVarBoolArray(ann, id, value)
            }
          case None => {
            addCPVarBoolArray(ann, id, l)
            
          }
           	 
        }
	}
	
	/**
	 * Creates a array of CPVarInt and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the array of var if it is assigned in the model
	 * @param id : the name of the array
	 * @param s : a set, the initial domain of the variables in the array
	 * @param ann : the list of annotation for the array of variables
	 */
	def createCPVarIntArray(e: Any, id: String, s: Set[Int], ann: List[Annotation], 
	    l: Int) {
	  e match {
          case Some("="~assign) =>
            assign match {
	      	  case x:List[Any] => 
	      	    model.dict += 
	      		((id, (FZType.V_ARRAY_INT, 
	      			new VarArrayInt(Set[Int](), ann, 
	      			    (x) map(d =>
      			    		  d match {
      			    		    case y:Int => 
      			    		      if((s contains y) || s.isEmpty) {
						      	    getCPVarInt(y)
						      	  } else {
						      	    throw new Exception(y + " not in the domain of " + id)
						      	  }
      			    		    case _ => 
      			    		      val cpvar = getCPVarInt(d)
						      	  if(!s.isEmpty) {
						      	    shrinkDom(s, cpvar)
						      	  }
						      	  cpvar
      			    		  }
	      			        ) toArray
	      		, id))))
	      	  case _ => 
	      	    var array = Array[CPVarInt]()
	      	    val value = getCPVarIntArray(assign)
	      	    value.foreach { cpvar =>
	      	      if(!s.isEmpty) {
		      	    shrinkDom(s, cpvar)
		      	  }
	      	      array :+= cpvar
	      	    }
	      	    addCPVarIntArray(ann, id, s, array)
	      	}
          case None => 
            addCPVarIntArray(ann, id, s, l)
          case _ => throw new Exception("Error in var int array creation")
        }
	}
	
	/**
	 * Creates a array of CPVarSet and adds it to the store
	 * @param e : the result of parsing an expr, represent the value of the array of set if it is assigned in the model
	 * @param id : the name of the array
	 * @param s : a set, the initial domain of the sets in the array
	 * @param ann : the list of annotation for the array of sets
	 */
	def createCPVarSetArray(e: Any, id: String, s: Set[Int], ann: List[Annotation], 
	    l: Int) {
	  e match {
	    case Some("="~assign) =>
	      assign match {
	        case x:List[Any] => 
	          model.dict += 
	      		((id, (FZType.V_ARRAY_SET, 
	      			new VarArraySet(s, ann, 
	      			    (x) map(d =>
      			    		  d match {
      			    		    case y:List[Int] => 
      			    		      // same as in varset creation, what is the assignment is {varint, varint} or {array(#),...}
      			    		      if(y.toSet.subsetOf(s)) { getCPVarSet(y) } 
      			    		      else {throw new Exception(y + " not in the domain of " + id)}
      			    		    case _ => 
      			    		      val cpvar = getCPVarSet(d)
						      	  shrinkDom(s, cpvar)
						      	  cpvar
      			    		  }
	      			    	) toArray
	      		, id))))
	        case _ =>
	          	var array = Array[CPVarSet]()
      	    	val value = getCPVarSetArray(assign)
      	    	value.foreach { cpvar =>
	      	    	if(!s.isEmpty) {
	      	    	  shrinkDom(s, cpvar)
	      	    	}
	      	    	array :+= cpvar
	          	}
	          	addCPVarSetArray(ann, id, s, array)
	      }
	    case None =>
	      addCPVarSetArray(ann, id, s, l)
	  }
	}
	
	/**
	 * Adds a CPVarBool to the store
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 */
	def addCPVarBool(ann: List[Annotation], id: String) {
	  model.dict += ((id, (FZType.V_BOOL, 
	      new VarBool(ann, CPVarBool()(cp), id))))
	}
	
	/**
	 * Adds a VarBool to the dictionnary of var, given a CPVarBool
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param cpvar : a cp variable
	 */
	def addCPVarBool(ann: List[Annotation], id: String, cpvar: CPVarBool) {
	  model.dict += ((id, (FZType.V_BOOL, new VarBool(ann, cpvar, id))))
	}
	
	/**
	 * Adds a CPVarInt to the store
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param s : the inital domain of the variable
	 * @param hasDomain : true of the inital domain is given
	 */
	def addCPVarInt(ann: List[Annotation], id: String, s: Set[Int]) {
	  if (!bool2Int.contains(id)) {
	    model.dict += ((id, (FZType.V_INT, 
	      new VarInt(ann, 
	          if(s.isEmpty) {
	            CPVarInt(UNDEFINED_VARINT_RANGE_MIN, UNDEFINED_VARINT_RANGE_MAX)(cp)
	          } else {
	            CPVarInt(s)(cp)
	          }
	      	, id))))
	  }
	}
	
	/**
	 * Adds a VarInt to the dictionnary of var, given a CPVarInt
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param cpvar : a cp variable
	 */
	def addCPVarInt(ann: List[Annotation], id: String, cpvar: CPVarInt) {
	  model.dict += ((id, (FZType.V_INT, new VarInt(ann, cpvar, id))))
	}
	/**
	 * Adds a CPVarSet to the store
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param s : the inital domain of the variable
	 */
	def addCPVarSet(ann: List[Annotation], id: String, s: Set[Int]) {
	  model.dict +=
        ((id, (FZType.V_SET_INT, 
            new VarSetInt(ann, CPVarSet(s)(cp), id))))
	}
	
	/**
	 * Adds a VarSetInt to the dictionnary of var, given a CPVarSet
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param cpvar : a cp variable
	 */
	def addCPVarSet(ann: List[Annotation], id: String, cpvar: CPVarSet) {
	  model.dict += ((id, (FZType.V_SET_INT, new VarSetInt(ann, cpvar, id))))
	}
	
	/**
	 * Adds an array of CPVarBool to the store
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param l : the length of the array
	 */
	def addCPVarBoolArray(ann: List[Annotation], id: String, l: Int) {
	  	val boolArray =  new VarArrayBool(ann, Array.fill(l){CPVarBool()(cp)}, id)
        model.dict += (id -> (FZType.V_ARRAY_BOOL, boolArray))
		// also add each individual entries
        for (i <- 1 to boolArray.cpvar.size) {
          model.dict += (id+"["+i+"]" -> (FZType.V_BOOL, new VarBool(ann,boolArray.cpvar(i-1),id)))
        }
	}
	
	/**
	 * Adds an array of VarBool to the dictionnary of var, given an array of CPVarBool
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param array : array of CPVarBool
	 */
	def addCPVarBoolArray(ann: List[Annotation], id: String, array: Array[CPVarBool]) {  
	  model.dict +=
        ((id, (FZType.V_ARRAY_BOOL, 
            new VarArrayBool(ann, array, id))))
            
            
	}
	/**
	 * Adds an array of CPVarInt to the store
	 * @param ann : the list of annotations for the variables
	 * @param id : the name of the array
	 * @param s : the inital domain of the variables
	 * @param l : the length of the array
	 */
	def addCPVarIntArray(ann: List[Annotation], id: String, s: Set[Int], 
	    l: Int) {
	  model.dict +=
      ((id, (FZType.V_ARRAY_INT,
        new VarArrayInt(s, ann, 
            if(s.isEmpty) {
            	Array.fill(l){CPVarInt(UNDEFINED_VARINT_RANGE_MIN, UNDEFINED_VARINT_RANGE_MAX)(cp)}
	          } else {
	            Array.fill(l){CPVarInt(s)(cp)}
	          }
        	, id))))
	}
	
	/**
	 * Adds an array of VarInt to the dictionnary of var, given an array of CPVarInt
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param array : array of CPVarInt
	 */
	def addCPVarIntArray(ann: List[Annotation], id: String, s: Set[Int], array: Array[CPVarInt]) {
	  model.dict +=
      ((id, (FZType.V_ARRAY_INT,
        new VarArrayInt(s, ann, array, id))))
	}
	
	/**
	 * Adds an array of CPVarSet to the store
	 * @param ann : the list of annotations for the variables
	 * @param id : the name of the array
	 * @param s : the inital domain of the variables
	 * @param l : the length of the array
	 */
	def addCPVarSetArray(ann: List[Annotation], id: String, s: Set[Int], l: Int) {
	  model.dict += 
      ((id, (FZType.V_ARRAY_SET, new VarArraySet(s, ann, Array.fill(l)(CPVarSet(s)(cp)), id))))
	}
	
	/**
	 * Adds an array of VarSet to the dictionnary of var, given an array of CPVarSet
	 * @param ann : the list of annotations for the variable
	 * @param id : the name of the variable
	 * @param array : array of CPVarSet
	 */
	def addCPVarSetArray(ann: List[Annotation], id: String, s: Set[Int], array: Array[CPVarSet]) {
	  model.dict += 
      ((id, (FZType.V_ARRAY_SET, 
          new VarArraySet(s, ann, array, id))))
	}
	
	/**
	 * Returns the length of x if x is a Range
	 * @param x
	 */
	def getRangeLength(x: Any): Int = {
	  x match {
	    case y:Range => y.length
	    case _ => 0
	  }
	}
	
	/**
	 * Shrinks the domain of a CPVarInt to the intersection of its domain and the set
	 * @param s : a set
	 * @param cpvar : CPVarInt
	 */
	def shrinkDom(s: Set[Int], cpvar: CPVarInt) {
	  if(cpvar.updateMax(s.max) == CPOutcome.Failure) { 
	    throw new NoSolutionException("VarInt domains are incompatible")
	  }
	  if(cpvar.updateMin(s.min)  == CPOutcome.Failure) { 
	    throw new NoSolutionException("VarInt domains are incompatible")
	  }
	  if(!(s.max - s.min + 1 == s.size)) {
	    for(e <- cpvar.domainIterator) {
	      if(!(s contains e)) {
	        if(cpvar.removeValue(e) == CPOutcome.Failure) {
	          throw new NoSolutionException("VarInt domains are incompatible")
	        }
	      }
	    }
	  }
	}
	
	/**
	 * Shrinks the domain of a CPVarSet to the intersection of its domain and the set
	 * @param s : a set
	 * @param cpvar : CPVarInt
	 */
	def shrinkDom(s: Set[Int], cpvar: CPVarSet) {
	  for(e <- cpvar.possibleNotRequiredValues.toSet[Int]) {
	    if(!(s contains e)) {
	      if(cpvar.excludes(e) == CPOutcome.Failure) { 
	        throw new NoSolutionException("Sets domains are incompatible")
	      }
	    }
	  }	
	  for(e <- cpvar.requiredValues.toSet[Int]) {
	    if(!(s contains e)) {
	      throw new NoSolutionException("Sets domains are incompatible")
	    }
	  }
	}
	
	/**
	 * Constraints parsing
	 */
	def constraint : Parser[Any] = "constraint"~pred_ann_id~"("~rep1sep(expr, ",")~")"~annotations~";" ^^ {
	  case "constraint"~cstr~"("~varList~")"~ann~";" => cstr match {
	    
	  	case "array_bool_and" =>
	      array_bool_cstr(varList, ann, cstr)
	  	case "array_bool_element" =>
	  	  System.err.println(cstr+" not implemented")
	    case "array_bool_or" =>
	      array_bool_cstr(varList, ann, cstr)
	      
	    case "array_int_element" =>
	      val b = getCPVarInt(varList(0))
	      val as = getIntArray(varList(1))
	      val c = getCPVarInt(varList(2))
	      addCstr(element(as, b-1, c), ann)
	      
	    case "array_var_bool_element" =>
	      val b = getCPVarInt(varList(0))
	      val as = getCPVarBoolArray(varList(1))
	      val c = getCPVarBool(varList(2))
	      addCstr(elementVar(as, b-1, c), ann)
	    case "array_var_int_element" =>
	      val b = getCPVarInt(varList(0))
	      val as = getCPVarIntArray(varList(1))
	      val c = getCPVarInt(varList(2))
	      addCstr(elementVar(as, b-1, c), ann)
	    case "bool2int" =>
	      //cp.add(getCPVarBool(varList(0)) == getCPVarInt(varList(1)))      
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
	      val CPArray = Array[CPVarInt](getCPVarInt(varList(0)), getCPVarInt(varList(1)))
	      addCstr(maximum(CPArray, getCPVarInt(varList(2))), ann)
	    case "int_min" =>
	      val CPArray = Array[CPVarInt](getCPVarInt(varList(0)), getCPVarInt(varList(1)))
	      addCstr(minimum(CPArray, getCPVarInt(varList(2))), ann)
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
	      val s = getCPVarSet(varList(0))
	      val i = getCPVarInt(varList(1))
	      addCstr(s.card == i, ann)
	    case "set_diff" =>
	      set_cstr(varList, ann, cstr)
	    case "set_eq" =>
	      set_cstr(varList, ann, cstr)
	    case "set_in" => {
	      val x = getCPVarInt(varList(0))
	      val s = getSetOfInt(varList(1))
	      for (v <- x.min to x.max; if !s.contains(v)) {
	        x.removeValue(v)
	      }
	    }      
	    // global constraints defined in minizinc/mznlib/
	    case "oscar_alldiff" =>
	      cp.add(allDifferent(getCPVarIntArray(varList(0))), Strong)
	    case "alldiff_0" =>
	    case "all_disjoint" =>
	    case "oscar_all_equal_int" =>
	      // to be tested
	      val array = getCPVarIntArray(varList(0))
	      for(i <- 0 to array.length - 2) {
	        addCstr(array(i) == array(i+1), ann)
	      }
	    case "oscar_among" =>
	      // no need to create the vals, can get while adding constraint, what is better ?
	      val n = getCPVarInt(varList(0))
	      val x = getCPVarIntArray(varList(1))
	      val s = getSetOfInt(varList(2))
	      addCstr(among(n, x, s), ann)
	    case "oscar_at_least_int" => 
	      // no need to create the vals, can get while adding constraint, what is better ?
	      val n = getInt(varList(0))
	      val x = getCPVarIntArray(varList(1))
	      val v = getInt(varList(2))
	      addCstr(atLeast(n, x, v), ann)
	    case "oscar_at most_int" =>
	      // no need to create the vals, can get while adding constraint, what is better ?
	      val n = getInt(varList(0))
	      val x = getCPVarIntArray(varList(1))
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
	      cp.add(circuit(getCPVarIntArray(varList(0)).map(_-1)),Strong)
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
	      val s = getCPVarIntArray(varList(0))
	      val d = getCPVarIntArray(varList(1))
	      val r = getCPVarIntArray(varList(2))
	      
	      //scheduler : CPScheduler, startVar : CPVarInt, durVar : CPVarInt, endVar : CPVarInt, resourceVar : CPVarInt, heightVar : CPVarInt,
	      
	      System.err.println("oscar_cumulative not implemented")
	    }
	    case "oscar_decreasing_int" =>
	      val array = getCPVarIntArray(varList(0))
	      for(i <- 0 to array.length - 2) {
	        addCstr(array(i) >= array(i+1), ann)
	      }
	    case "oscar_diffn" =>
	      val x = getCPVarIntArray(varList(0))
	      val y = getCPVarIntArray(varList(1))
	      val dx = getCPVarIntArray(varList(2))
	      val dy = getCPVarIntArray(varList(3))
	      for(i <- 0 until x.length; j <- i+1 until x.length) {
	        cp.add( 
	            ((x(i) + dx(i) <== x(j)) || (x(j)+dx(j) <== x(i))) ||
	            ((y(i) + dy(i) <== y(j)) || (y(j)+dy(j) <== y(i)))
	        ) 
	      }
	    case "oscar_disjoint" =>
	      addCstr(disjoint(getCPVarSet(varList(0)),getCPVarSet(varList(1))), ann)
	    case "oscar_distribute" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_element_bool" =>
	      System.err.println(cstr+" not implemented")
	      //cp.add(elementVar(getCPVarIntArray(varList(1).toString), getCPVarInt(varList(0)), getCPVarInt(varList(2))))
	    case "oscar_element_int" =>
	      addCstr(elementVar(getCPVarIntArray(varList(1)), getCPVarInt(varList(0)), getCPVarInt(varList(2))), ann)
	    case "exactly_int" => //not used, done with among
	      
	    case "oscar_global_cardinality" =>
	      gcc_cstr(varList)
	      
	    case "oscar_global_cardinality_closed" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_global_cardinality_low_up" => 
	      gcc_lbub_cstr(varList)
	    case "oscar_global_cardinality_low_up_closed" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_increasing_int" =>
	      val array = getCPVarIntArray(varList(0))
	      for(i <- 0 to array.length - 2) {
	        addCstr(array(i) <= array(i+1), ann)
	      }
	    case "oscar_int_set_channel" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_inverse" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_inverse_set" =>
	      System.err.println(cstr+" not implemented")
	    case "lex_greater_int" => //not used, done with lex_less
	      System.err.println(cstr+" not implemented")
	    case "lex_greatereq_int" => //not used, done with lex_lesseq
	      System.err.println(cstr+" not implemented")
	    case "oscar_lex_less_int" =>
	      val t1 = getCPVarIntArray(varList(0))
	      val t2 = getCPVarIntArray(varList(1))
	      addCstr(lexLeq(t1, t2), ann)
	      diff_array_cstr(t1, t2)
	    case "oscar_lex_lesseq_int" =>
	      addCstr(lexLeq(getCPVarIntArray(varList(0)), getCPVarIntArray(varList(1))), ann)
	    case "oscar_lex2" => //2D -> 1D done, need to parse the constraint
	      lex2_cstr(varList, false)
	    case "oscar_link_set_to_booleans" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_maximum_int" =>
	      addCstr(maximum(getCPVarIntArray(varList(1)), getCPVarInt(varList(0))), ann)
	    case "oscar_member_int" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_minimum_int" =>
	      addCstr(minimum(getCPVarIntArray(varList(1)), getCPVarInt(varList(0))), ann)
	    case "oscar_nvalue" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_partition_set" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_range" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_regular" => //2D -> 1D done
	      regular_cstr(varList)
	    case "oscar_roots" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_sliding_sum" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_sort" => 
	      sort_cstr(varList)
	    case "oscar_strict_lex2" =>
	      lex2_cstr(varList, true)
	    case "oscar_subcircuit" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_sum_pred" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_table_int" => //2D -> 1D done
	      table_cstr(varList)
	    case "oscar_value_precede_int" =>
	      System.err.println(cstr+" not implemented")
	    case "oscar_value_precede_chain_int" =>
	      System.err.println(cstr+" not implemented")
	  }
	}
	
	/**
	 * Bin packing constraints
	 * @param varList : a list of the arguments for the constraint
	 * @param tp : the type of bin packing to use
	 */
	def bin_packing(varList: List[Any], tp: String) {
	  val l =
	  tp match {
	    case "def" => 
	      val nbBin = getCPArrayRangeSize(varList(1).toString)
	      Array.fill(nbBin){CPVarInt(0, varList(0).toString.toInt)(cp)}
	    case "load" => getCPVarIntArray(varList(0))
	    case "capa" =>
	      var capaCP = Array[CPVarInt]()
	      val capaInt = getIntArray(varList(0).toString)
	      capaInt.foreach { e =>
	        capaCP :+= CPVarInt(0, e)(cp)
	      }
	      capaCP
	  }
	  cp.add(binpacking(getCPVarIntArray(varList(1)).map(_-1), 
	          getIntArray(varList(2)), l),Strong)
	}
	
	/**
	 * Count constraints
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the particular count constraint
	 */
	def count_cstr(varList: List[Any], ann: Any, cstr: String) {
      val x = getCPVarIntArray(varList(0))
      val y = getCPVarInt(varList(1))
      val n = getCPVarInt(varList(2))
	  cstr match {
	    case "oscar_count_eq" => addCstr(countEq(n, x, y))
	    case "oscar_count_geq" => addCstr(countGeq(n, x, y))
	    case "oscar_count_gt" => addCstr(countGt(n, x, y))
	    case "oscar_count_leq" => addCstr(countLeq(n, x, y))
	    case "oscar_count_lt" => addCstr(countLt(n, x, y))
	    case "oscar_count_neq" => addCstr(countNeq(n, x, y))
	  }
	}
	
	/**
	 * Lex2 constraints
	 * @param varList : a list of the arguments for the constraint
	 * @param strict : if the lexical order must be strict or not
	 */
	def lex2_cstr(varList: List[Any], strict: Boolean) {
	  //could maybe be done by recreating the orginal array and working on it...
      val rows = varList(1).toString.toInt
      val cols = varList(2).toString.toInt
      var array = Array[CPVarInt]()
      varList(0) match {
        case x:List[Any] => {
          x.foreach{ e => 
		  	e match {
			    case x:List[Any] => array :+= getCPVarIntFromList(x)
		  	}
          }
        }
      }
      var x = Array[CPVarInt]()
      var y = Array[CPVarInt]()
      //rows
      //index = i*cols + j
      for( i <- 0 until rows-1; j <- 0 to cols-1){
        x :+= array(i*cols+j)
        y :+= array((i+1)*cols+j)
        if(j == cols-1) {
          //posting the constraint for two consecutive rows and reseting the arrays
          addCstr(lexLeq(x, y))
          if(strict) {
            diff_array_cstr(x, y)
          }
          x = Array[CPVarInt]()
          y = Array[CPVarInt]()
        }
      }
      //cols
      //intx : = j*cols + i
      for( i <- 0 until cols-1; j <- 0 to rows-1){
        x :+= array(j*cols+i)
        y :+= array(j*cols+i+1)
        if(j == rows-1) {
          //posting the constraint for two consecutive rows and reseting the arrays
          addCstr(lexLeq(x, y))
          if(strict) {
            diff_array_cstr(x, y)
          }
          x = Array[CPVarInt]()
          y = Array[CPVarInt]()
        }
      }
	}
	
	/**
	 * Global cardinality constraint
	 * @param varList : a list of the arguments for the constraint
	 */
	def gcc_cstr(varList: List[Any]) {
      val cover = getIntArray(varList(1))
      val count = getCPVarIntArray(varList(2))
      assert(cover.length == count.length, "Count has not the same size as cover")
      var valueOccurrence = Array[(Int, CPVarInt)]()
      for(i <- 0 until cover.length) {
        valueOccurrence :+= (cover(i), count(i))
      }
      val x = getCPVarIntArray(varList(0))
      addCstr(gcc(x, valueOccurrence))
	}
	
	/**
	 * Global cardinality with lower and upper bound
	 * @param varList : a list of the arguments for the constraint
	 */
	def gcc_lbub_cstr(varList: List[Any]) {
	  // to be tested
      val cover = getIntArray(varList(1))
      val lb = getIntArray(varList(2))
      val ub = getIntArray(varList(3))
      val x = getCPVarIntArray(varList(0))
      assert(cover.length == lb.length, "Cover has not the same size as lb")
      assert(cover.length == ub.length, "Cover has not the same size as ub")
      val r = Range(cover.min, cover.max+1)
      var min = Array.fill(r.size){0}
      var max = Array.fill(r.size){x.length}
      // Array.fill(x.length){CPVarBool()(cp)} 
      for(i <- 0 until cover.length) {
        min(cover(i)-cover.min) = lb(i)
        max(cover(i)-cover.max) = ub(i)
      }
      addCstr(gcc(x, r, min, max))
	}
	
	/**
	 * Sort constraint
	 * @param varList : a list of the arguments for the constraint
	 */
	def sort_cstr(varList: List[Any]) {
	  val x = getCPVarIntArray(varList(0))
	  val y = getCPVarIntArray(varList(1))
	  val perm = Array.fill(x.size)(CPVarInt(0 until x.size)(cp))
	  cp.add(sortedness(x,y,perm))
	}
	
	/**
	 * Regular constraint
	 * @param varList : a list of the arguments for the constraint
	 */
	def regular_cstr(varList: List[Any]) {
	  var set : java.util.Set[Integer] = new java.util.TreeSet[Integer]()
      varList(5) match {
        case x:Range => 
          x.foreach{ e =>
          	set.add(e-1)
          }
        case x:List[Any] =>
          x.foreach{ e =>
          	set.add(e.toString.toInt-1)
          }
      }
      val Q = varList(1).toString.toInt
      val S = varList(2).toString.toInt
      val q0 = varList(4).toString.toInt-1
      val x = getCPVarIntArray(varList(0)).map(_-1)
      val a = new Automaton(Q, S, q0, set)
      var next = 0
      for(q <- 0 until Q; s <- 0 until S) {
        next = varList(3).asInstanceOf[List[Int]](q*S + s)
        if(next != 0){
          a.addTransition(q, next-1, s)
        }
      }
      addCstr(regular(x, a))
	}
	
	/**
	 * Table constraint
	 * @param varList : a list of the arguments for the constraint
	 */
	def table_cstr(varList: List[Any]) {
	  val CPArray = getCPVarIntArray((varList(0)))
      val tupleLength = CPArray.length
      val intArray = varList(1).asInstanceOf[List[Int]]
      var temp = Array[Int]()
      var tuples = Array[Array[Int]]()
      for( i <- 0 to (intArray.length/tupleLength)-1){
        for( j <- 0 to tupleLength-1) {
          temp :+= intArray(i*tupleLength + j)
        }
        tuples :+= temp
        temp = Array[Int]()
      }
      addCstr(table(CPArray, tuples))
	}
	
	/**
	 * Constraint specifying that two arrays must be different
	 * @param x, y
	 */
	def diff_array_cstr(x: Array[CPVarInt], y: Array[CPVarInt]) {
	  addCstr(sum(x) != sum(y))
	}
	
	/**
	 * Adds a constraint to the store with the specified propagation strength
	 * @param c : constraint
	 * @param ann : list of annotations in which the strengh may be specified
	 */
	/*
	 * TODO : to take the constraint annotations into account, 
	 * all the "cp.add(cstr)" must be replaced by "addCstr(cstr, ann)"
	 * ann or the variable representing the list of annotations
	 */
	/*
	 * For it to work with global constraints, the list must be given 
	 * as arg when calling the functions responsible for the constraints
	 */
	def addCstr(c: Constraint, ann: List[Annotation] = List[Annotation]()) {
	  assert(ann.length <= 1, "One annotation max on constraint")
	  try {
		  if(ann.length > 0) {
			  ann(0).name match {
			    case "domain" => cp.add(c, Strong)
			    case "boundsR" => cp.add(c, Medium)
			    case "boundsD" => cp.add(c, Medium)
			    case _ => cp.add(c)
			  }
		  } else {
		    cp.add(c)
		  }
	  } catch {
	      case e:NoSolutionException => println("=====UNSATISFIABLE=====")
	      case _:Throwable => throw new Exception("adding the constraint failed")
	  }
	}
	
	/**
	 * Constraints on arrays of booleans
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def array_bool_cstr(varList: List[Any], ann: List[Annotation], cstr: String){
	  cstr match {
	    case "array_bool_element" =>
	    case _ => {
	      val array = getCPVarBoolArray(varList(0))
	      cstr match {
	        case "array_bool_xor" =>
	        case _ =>
	          val boolvar = getCPVarBool(varList(1))
	          cstr match {
	            case "array_bool_and" => {
	              addCstr(new oscar.cp.constraints.And(array,boolvar))
	              //cp.add(new GrEqVarReif(sum(array), CPVarInt(cp, array.length), boolvar))
	            }
	            case "array_bool_or" => addCstr(new Or(array, boolvar))
	          }
	      }
	    }
	  }
	}
	
	/**
	 * Constraints on booleans
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def bool_cstr(varList: List[Any], ann: List[Annotation], cstr: String) {
	  var cpvar = Array[CPVarBool]()
	  varList.foreach{ e =>
	    cpvar :+= getCPVarBool(e)
	  }
	  cstr match {
	    case "bool_and" => addCstr((cpvar(0) && cpvar(1)) == cpvar(2))
	    case "bool_eq" => addCstr(cpvar(0) == cpvar(1))
	    case "bool_eq_reif" => addCstr(new EqReifVar(cpvar(0), cpvar(1), cpvar(2)))
	    case "bool_le" => addCstr(cpvar(0) <= cpvar(1))
	    case "bool_le_reif" => addCstr(new GrEqVarReif(cpvar(1),cpvar(0),cpvar(2)))
	    case "bool_lt" => {
	      addCstr(cpvar(0) == 0)
	      addCstr(cpvar(1) == 1)
	      //cp.add(!cpvar(0) && cpvar(1))
	    }
	    case "bool_lt_reif" => {
	      addCstr(new GrEqVarReif(cpvar(1)-1,cpvar(0),cpvar(2)))
	      //cp.add((!cpvar(0) && cpvar(1)) == cpvar(2))
	    }
	    case "bool_not" => addCstr(!cpvar(0) == cpvar(1))
	    case "bool_or" => addCstr((cpvar(0) || cpvar(1)) == cpvar(2))
	    case "bool_xor" => addCstr(new DiffReifVar(cpvar(0), cpvar(1), cpvar(2)))
	  }
	}
	
	/**
	 * Constraints on integers
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def int_cstr(varList: List[Any], ann: List[Annotation], cstr: String) {
	  var cpvar = Array[CPVarInt]()
	  varList.foreach{ e => 
	    cpvar :+= getCPVarInt(e)
	  }
	  cstr match {
	    case "int_abs" => addCstr(new Abs(cpvar(0), cpvar(1)))
	    case "int_eq" => addCstr(cpvar(0) == cpvar(1))
	    case "int_le" => 
	      //addCstr(cpvar(0) <= cpvar(1), ann) //example of adding constraint with annotation
	      addCstr(cpvar(0) <= cpvar(1))
	    case "int_lt" => addCstr(cpvar(0) < cpvar(1))
	    case "int_ne" => addCstr(cpvar(0) != cpvar(1))
	    case "int_plus" => addCstr(cpvar(0) + cpvar(1) == cpvar(2))
	    case "int_times" => addCstr(cpvar(0) * cpvar(1) == cpvar(2))
	  }
	}
	
	/**
	 * Reified constraints on integers
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def int_reif(varList: List[Any], ann: List[Annotation], cstr: String) {
	  var cpvar = Array[CPVarInt]()
	  for (i <- 0 until varList.size-1) {
	    cpvar :+= getCPVarInt(varList(i))
	  }
	  val boolvar = getCPVarBool(varList.last)
	  cstr match {
	    case "int_eq_reif" => addCstr(new EqReifVar(cpvar(0), cpvar(1), boolvar))
	    case "int_le_reif" => addCstr(new GrEqVarReif(cpvar(1), cpvar(0), boolvar))
	    case "int_lt_reif" => addCstr(new GrEqVarReif(cpvar(1), cpvar(0)+1, boolvar))
	    case "int_ne_reif" => addCstr(new DiffReifVar(cpvar(0), cpvar(1), boolvar))
	  } 
	}
	
	/**
	 * Constraints involving arrays of ints or bools
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def int_lin_cstr(varList: List[Any], ann: List[Annotation], cstr: String) {
	  var cpvar = Array[CPVarInt]()
	  if(cstr == "bool_lin_eq" || cstr == "bool_lin_le") {
	    cpvar = getCPVarBoolArray(varList(1)).map(_.asInstanceOf[CPVarBool])
	  } else {
	    cpvar = getCPVarIntArray(varList(1))
	  }
	  
	  var cst = getIntArray(varList(0))
	  val c = getInt(varList(2))

      cstr match {
        case "int_lin_ne" => 
          addCstr(weightedSum(cst, cpvar) != c)
        case "int_lin_eq" => {
          if (c == 0 && cst(0) == -1 && cst.tail.forall(_==1)) {
            // sum constraint
            //System.err.println("int_lin_eq, sum identified")
            addCstr(sum(cpvar.tail,cpvar.head))
          } else if (c == 0 && cst(0) == 1 && cst.tail.forall(_ == -1)) {
            //System.err.println("int_lin_eq, sum identified")
            addCstr(sum(cpvar.tail,cpvar.head))            
          } else if (c == 0 && cst.last == -1 && cst.reverse.tail.forall(_ == 1)) {
            //System.err.println("int_lin_eq, sum identified")
            addCstr(sum(cpvar.reverse.tail,cpvar.last))
          }
          else if (c == 0 && cst.last == 1 && cst.reverse.tail.forall(_ == -1)) {
            //System.err.println("int_lin_eq, sum identified")
            addCstr(sum(cpvar.reverse.tail,cpvar.last))
          }
          else {
            addCstr(weightedSum(cst,cpvar,c))
          }
          
        }
        case "int_lin_le" => 
          addCstr(weightedSum(cst, cpvar) <= c, ann) 
        case "int_lin_eq_reif" => 
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
        case "int_lin_le_reif" =>
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
        case "int_lin_ne_reif" =>
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
          
        case "bool_lin_eq" => 
          addCstr(weightedSum(cst, cpvar) == c)
        case "bool_lin_le" => 
          addCstr(weightedSum(cst, cpvar) <= c)
      }
	}
	
	/**
	 * Reified constraints involving arrays of ints and bools
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def int_lin_reif_cstr(cpvar: Array[CPVarInt], cst: Array[Int], c: Int, 
	    varList: List[Any], ann: List[Annotation], cstr: String) {
      val boolvar = getCPVarBool(varList(varList.length-1))
      cstr match {
        case "int_lin_eq_reif" => {
          //cp.add(new WeightedSum(cst,cpvar,c))
          //cp.add(new EqReif(weightedSum(cst, cpvar), c, boolvar))
          //cp.add(new oscar.cp.constraints.WeightedSumReif(cst,cpvar,c,boolvar))
          addCstr(new EqReif(weightedSum(cst, cpvar), c, boolvar))
        }
        case "int_lin_le_reif" =>
          addCstr(new GrEqCteReif(weightedSum(cst.map(-_),cpvar),-c, boolvar))
        case "int_lin_ne_reif" =>
          addCstr(new DiffReif(weightedSum(cst, cpvar), c, boolvar))
      }
	}
	
	/**
	 * Constraints on sets
	 * @param varList : a list of the arguments for the constraint
	 * @param ann : list of annotations for the constraint
	 * @param cstr : the constraint to add
	 */
	def set_cstr(varList: List[Any], ann: List[Annotation], cstr: String) {
	  var cpvar = Array[CPVarSet]()
	  varList.foreach{ e => 
	    cpvar :+= getCPVarSet(e)
	  }
	  cstr match {
	    case "set_diff" =>
	      addCstr(new SetDiff(cpvar(0), cpvar(1), cpvar(2)))
	    case "set_eq" => addCstr(cpvar(0) == cpvar(1))
	  }
	}
	
	/**
	 * Returns a boolean parameter
	 * @param x : a boolean or the name of a known parameter
	 * @return a boolean
	 */
	def getBool(x:Any): Boolean = {
	  x match {
	    case y:Boolean => y
        case y:String => 
          model.dict.get(y) match {
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
	
	/**
	 * Returns a integer parameter
	 * @param x : a integer or the name of a known parameter
	 * @return a integer
	 */
	def getInt(x:Any): Int = {
	  x match {
	    case y:Int => y
        case y:String => 
          model.dict.get(y) match {
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
	
	/**
	 * Returns an array of booleans
	 * @param x : a boolean array or the name of a known array of booleans
	 * @return an array of booleans
	 */
	def getBoolArray(x: Any): Array[Boolean] = {
	  x match {
	    case y:List[Any] => y.asInstanceOf[List[Boolean]].toArray
        case y:String => 
          model.dict.get(y) match {
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
	
	/**
	 * Returns an array of integers
	 * @param x : a integer array or the name of a known array of integers
	 * @return an array of integers
	 */
	def getIntArray(x: Any): Array[Int] = {
	  x match {
	    case y:List[Any] => y.asInstanceOf[List[Int]].toArray
        case y:String => 
          model.dict.get(y) match {
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
	
	/**
	 * Returns an set
	 * @param x : a set, a range, or the name of a known set
	 * @return a set
	 */
	def getSetOfInt(x:Any): Set[Int] = {
	  x match {
	    case y:Range => y.toSet[Int]
	    case y:List[Int] => y.toSet[Int]
        case y:String => 
          model.dict.get(y) match {
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
	
	/**
	 * Returns a CPVarBool
	 * @param x : a boolean or the name of a known CPVarBool (can be a variable in an array)
	 * @return CPVarBool
	 */
	def getCPVarBool(x: Any): CPVarBool = {
	  x match {
	    case x:List[Any] => getCPVarBoolFromList(x)
	    case x:String => getCPVarBoolFromString(x)
	    case x:Boolean => CPVarBool(x)(cp)
	  }
	}
	
	/**
	 * Returns a CPVarInt
	 * @param x : an integer or the name of a known CPVarInt (can be a variable in an array)
	 * @return CPVarBool
	 */
	def getCPVarInt(x: Any): CPVarInt = {
	  x match {
	    case x:Int => CPVarInt(x)(cp)
	    case x:List[Any] => getCPVarIntFromList(x)
	    case x:String => getCPVarIntFromString(x)
	  }
	}
	
	/**
	 * Returns a CPVarSet
	 * @param x : a set or the name of a known CPVarSet
	 * @return CPVarSet
	 */
	def getCPVarSet(x: Any): CPVarSet = {
	  x match {
	    case x:List[Any] => getCPVarSetFromList(x)
	    case x:String => getCPVarSetFromString(x)
	  }
	}
	
	/**
	 * Returns a CPVarBool
	 * @param x : the name of a known CPVarBool
	 * @return CPVarBool
	 */
	def getCPVarBoolFromString(x: String): CPVarBool = {
      model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.V_BOOL => {
	              fzo.asInstanceOf[VarBool].cpvar
	            }
	            case FZType.P_BOOL => {
	              CPVarBool(fzo.asInstanceOf[ParamBool].value)(cp)
	            }
	        }
	      case None => throw new Exception("Var " + x + " does not exist")
	    }
	}
	
	/**
	 * Returns a CPVarInt
	 * @param x : the name of a known CPVarInt
	 * @return CPVarInt
	 */
	def getCPVarIntFromString(x: String): CPVarInt = {
	  //if (bool2Int.contains(x)) System.err.println("mmmh, I know this bool")
	  model.dict.get(bool2Int.getOrElse(x, x)) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.V_INT => {
	              fzo.asInstanceOf[VarInt].cpvar
	            }
	            case FZType.V_BOOL => {
	              //System.err.println("found it!!")
	              fzo.asInstanceOf[VarBool].cpvar
	            }
	            case FZType.P_INT => {
	              CPVarInt(fzo.asInstanceOf[ParamInt].value)(cp)
	            }
	            case _ => {
	              throw new Exception("Var " + x + " does not match")
	            }
	        }
	      case None => throw new Exception("Var " + x + " does not exist")
	    }
	}
	
	/**
	 * Returns a CPVarSet
	 * @param x : the name of a known CPVarSets
	 * @return CPVarSet
	 */
	def getCPVarSetFromString(x: String): CPVarSet = {
	  model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.V_SET_INT => {
	              fzo.asInstanceOf[VarSetInt].cpvar
	            }
	            case FZType.P_SET_INT => {
	              CPVarSet(fzo.asInstanceOf[ParamSetOfInt].value)(cp)
	            }
	        }
	      case None => throw new Exception("Var " + x + " does not exist")
	    }
	}
	
	/**
	 * Returns a CPVarBool from an array
	 * @param x : a list with the name of an array and an index
	 * @return CPVarBool
	 */
	def getCPVarBoolFromList(x: List[Any]): CPVarBool = {
	  model.dict.get(x(0).toString) match {
          case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_BOOL => {
                  fzo.asInstanceOf[VarArrayBool].cpvar(x(1).toString.toInt-1)
                }
            }
          case None => throw new Exception("Var " + x + " does not exist")
        }
	}
	
	/**
	 * Returns a CPVarInt from an array
	 * @param x : a list with the name of an array and an index
	 * @return CPVarInt
	 */
	def getCPVarIntFromList(x: List[Any]): CPVarInt = {
	  model.dict.get(x(0).toString) match {
          case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_INT => {
                  fzo.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
                }
            }
          case None => throw new Exception("Var " + x + " does not exist")
        }
	}
	
	/**
	 * Returns a CPVarSet
	 * @param x : a list with the name of an array and an index or an array of integer
	 * @return CPVarInt
	 */
	def getCPVarSetFromList(x: List[Any]): CPVarSet = {
	  x(0) match {
	    case y:Int => 
	      // x is here a requiered set for the cpvarset
	      CPVarSet(x map(_.toString.toInt) toSet)(cp)
	    case y:String => 
	      model.dict.get(y) match {
		      case Some((tp, fzo)) => 
		        tp match {
		            case FZType.V_ARRAY_SET => {
		              fzo.asInstanceOf[VarArraySet].cpvar(x(1).toString.toInt-1)
		            }
		        }
		      case None => throw new Exception("Var " + x + " does not exist")
		    }
	  }
	}
	
	/**
	 * Returns an array of CPVarBool
	 * @param x : a array of boolean or the name of a known array of CPVarBool
	 * @return an array of CPVarBool
	 */
	def getCPVarBoolArray(x: Any): Array[CPVarBool] = {
	  x match {
	    case y:List[Any] =>
	      var array = Array[CPVarBool]()
	      y.foreach { e =>
	      	array :+= getCPVarBool(e)
	      }
	      array
	    case y:String =>
	      model.dict.get(y) match {
			  case Some((tp, fzo)) => 
	            tp match {
	                case FZType.V_ARRAY_BOOL => {
	                  fzo.asInstanceOf[VarArrayBool].cpvar
	                }
	                case FZType.P_ARRAY_BOOL => {
	                  val array = fzo.asInstanceOf[ParamArrayBool].value
	                  array match {
	                    case x:List[Boolean] =>
	                      (x) map(CPVarBool(_)(cp)) toArray
	                  }
	                }
	            }
	          case None => throw new Exception("Var " + x + " does not exist")
	      }
	  }
	}
	
	/**
	 * Returns an array of CPVarInt
	 * @param x : a array of integer or the name of a known array of CPVarInt
	 * @return an array of CPVarInt
	 */
	def getCPVarIntArray(x: Any): Array[CPVarInt] = {
	  x match {
	    case y:List[Any] =>
	      (y) map(getCPVarInt(_)) toArray
	    case y:String =>
	      model.dict.get(y) match {
			  case Some((tp, fzo)) => 
	            tp match {
	                case FZType.V_ARRAY_INT => {
	                  fzo.asInstanceOf[VarArrayInt].cpvar
	                }
	                case FZType.P_ARRAY_INT => {
	                  val array = fzo.asInstanceOf[ParamArrayInt].value
	                  array match {
	                    case x:List[Int] =>
	                      (x) map(CPVarInt(_)(cp)) toArray
	                  }
	                }
	            }
	          case None => throw new Exception("Var " + y + " does not exist")
	      }
	  }
	}
	
	/**
	 * Returns an array of CPVarSet
	 * @param x : a array of sets or the name of a known array of CPVarSet
	 * @return an array of CPVarBool
	 */
	def getCPVarSetArray(x: Any): Array[CPVarSet] = {
	  x match {
	    //need to test, may not work in a case :
	    // [ {1, 2, 4}, {3, 45, x} ] with x a CPVarInt
	    case y:List[List[Int]] =>
	      (y) map(d => CPVarSet(d.toSet)(cp)) toArray
	    case y:String =>
	      model.dict.get(y) match {
			  case Some((tp, fzo)) => 
	            tp match {
	                case FZType.V_ARRAY_SET => {
	                  fzo.asInstanceOf[VarArraySet].cpvar
	                }
	                case FZType.P_ARRAY_SET_INT => {
	                  val array = fzo.asInstanceOf[ParamArraySetOfInt].value
	                  array match {
	                    case x:List[Any] =>
	                      (x) map(d => 
	                      	d match {
	                      	  case y:List[Any] => 
	                      	    getCPVarSetFromList(y)
	                      	    //println("YOUPI")
	                      	}
	                      ) toArray
	                  }
	                }
	            }
	          case None => throw new Exception("Var " + y + " does not exist")
	      }
	  }
	}
	
	/**
	 * Returns the size of an array of CPVarInt
	 * @param x : a known array of CPVarInt
	 * @return the length of the array
	 */
	def getCPArrayRangeSize(x: String): Int = {
	  model.dict.get(x) match {
		  case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_INT => {
                  fzo.asInstanceOf[VarArrayInt].value.size
                }
            }
          case None => throw new Exception("Var " + x + " does not exist")
	  }
	}
	
	/**
	 * Returns the indexes of an array of var as given in the annotation in the model
	 * Is used to format the output correctly
	 * @param x : the name of a known array of CPVar
	 * @return the list of indexes
	 */
	def getCPArrayOutputAnnotations(x: String): List[Range] = {
		var l = List[Range]()
		model.dict.get(x) match {
		  case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_BOOL => {
                  for (ann <- fzo.asInstanceOf[VarArrayBool].annotations 
                      if (ann.name == "output_array")) {
                      	l = ann.args.asInstanceOf[List[List[Range]]](0)   	
                  }
                }
                case FZType.V_ARRAY_INT => {
                  for (ann <- fzo.asInstanceOf[VarArrayInt].annotations 
                      if (ann.name == "output_array")) {
                      	l = ann.args.asInstanceOf[List[List[Range]]](0)
                  }
                }
                case FZType.V_ARRAY_SET => {
                  for (ann <- fzo.asInstanceOf[VarArraySet].annotations 
                      if (ann.name == "output_array")) {
                      	l = ann.args.asInstanceOf[List[List[Range]]](0)
                  }
                }
            }
          case None => throw new Exception("Var " + x + " does not exist")
		}
		l
	}
	
	/**
	 * From a list with the type any (due to the parsing), returns the set of int that is represented
	 * @param intList : represents a list of int
	 * @return a set of int
	 */
	def getSetFromList(intList: Any): Set[Int] = {
	  	intList match {
	        case x:List[Int] => 
	          x.toSet[Int]
	      }
	}
	
	def solve_goal : Parser[Any] = (
	    "solve"~annotations~"satisfy;" ^^ { 
	      case "solve"~ann~"satisfy;" => solver("sat", null, ann)
	    }
	    | "solve"~annotations~"minimize"~expr~";" ^^ { 
	      case "solve"~ann~"minimize"~e~";" => solver("min", e, ann)
	    }
	    | "solve"~annotations~"maximize"~expr~";" ^^ { 
	      case "solve"~ann~"maximize"~e~";" => solver("max", e, ann)
	    }
	)
	
	/**
	 * Gets all the variable to be assigned by the solver depending on the search annotations
	 * Performs the search and output the results
	 * @param tp : the type of search : satisfy, maximize or minimize
	 * @param expr : the variable that must be optimized in the case of an optimization problem
	 * @param ann : the list of annotations related to the search
	 */
	def solver(tp: String, expr: Any, ann: List[Annotation]) {
	  var x = Array[CPVarInt]()
	  var s = Array[CPVarSet]()
	  // array with all the variable so that it is possible to output correctly
	  var xs = Array[CPVar]()
	  var state = Array[VarState]()
	  var setstate = Array[VarState]()
	  var xsstate = Array[VarState]()
	  if(true) { 
	      var output: Boolean = false // only used for formating the output
	      model.dict.toSeq.sortBy(_._1) foreach {
	        // the condition that key mustn't contain "[" is to avoid adding twice the CPVarBool created to avoid the bool2int constraints
		    case (key, value) if(!(key contains "[")) =>
		     value match {
	          case (tp, fzo) => // /!\ not always CPVarInt
	            tp match {
	              case FZType.V_BOOL => {
	                val obj = fzo.asInstanceOf[VarBool]
	                x :+= obj.cpvar
	                xs :+= obj.cpvar
	                obj.annotations.foreach { ann =>
	            		if ( ann.name == "output_var" ) { output = true }
	                }
	                xsstate :+= new VarState(obj.name,
	                    output, false, false, false, 1, FZType.V_BOOL)
	                output = false
	              }
	              case FZType.V_ARRAY_BOOL => {
	                val obj = fzo.asInstanceOf[VarArrayBool]
	                var first = true
	                var last = false
	                obj.cpvar.foreach { e =>
	                  x :+= e
	                  xs :+= e
	                  obj.annotations.foreach { ann =>
            			if ( ann.name == "output_array" ) { 
            			  ann.args match {
            			    case y:List[List[Range]] => 
            			      	output = true
            			      	if (e == obj.cpvar.last){
            			      	  last = true
            			      	}
            			  }
            			}
	                  }
	                  xsstate :+= new VarState(obj.name,
	                		  output, true, first, last, 
	                		  obj.cpvar.length, FZType.V_ARRAY_BOOL)
	                  if(output) {
	                    first = false
	                  }
	                  output = false
	                  last = false
	                }
	                output = false
	              }
	                
	              case FZType.V_INT => {
	                val obj = fzo.asInstanceOf[VarInt]
	                x :+= obj.cpvar
	                xs :+= obj.cpvar
	                obj.annotations.foreach { ann =>
	            		if ( ann.name == "output_var" ) { output = true }
	                }
	                xsstate :+= new VarState(obj.name,
	                    output, false, false, false, 1, FZType.V_INT)
	                output = false
	              }
	              case FZType.V_ARRAY_INT => {
	                val obj = fzo.asInstanceOf[VarArrayInt]
	                var first = true
	                var last = false
	                obj.cpvar.foreach { e =>
	                	x :+= e
	                	xs :+= e
	                	obj.annotations.foreach { ann =>
	            			if ( ann.name == "output_array" ) { 
	            			  ann.args match {
	            			    case y:List[List[Range]] => 
	            			      	output = true
	            			      	if (e == obj.cpvar.last){
	            			      	  last = true
	            			      	}
	            			  }
	            			}
	                	}
	                	xsstate :+= new VarState(obj.name,
	                			output, true, first, last, 
	                			obj.cpvar.length, FZType.V_ARRAY_INT)
	                	if(output) {
	                		first = false
	                	}
	                	output = false
	                	last = false
	                }
	                output = false
	              }
	              case FZType.V_SET_INT => {
	                val obj = fzo.asInstanceOf[VarSetInt]
	                s :+= obj.cpvar
	                xs :+= obj.cpvar
	                obj.annotations.foreach { ann =>
	            		if ( ann.name == "output_var" ) {
	            		  output = true 
	            		}
	                }
	                xsstate :+= new VarState(obj.name,
	                    output, false, false, false, 1, FZType.V_SET_INT)
	                output = false
	              }
	              case FZType.V_ARRAY_SET => {
	                val obj = fzo.asInstanceOf[VarArraySet]
	                var first = true
	                var last = false
	                obj.cpvar.foreach { e =>
	                	s :+= e
	                	xs :+= e
	                	obj.annotations.foreach { ann =>
	            			if ( ann.name == "output_array" ) { 
	            			  ann.args match {
	            			    case y:List[List[Range]] => 
	            			      	output = true
	            			      	if (e == obj.cpvar.last){
	            			      	  last = true
	            			      	}
	            			  } 
	            			  
	            			}
	                	}
	                	xsstate :+= new VarState(obj.name,
	                			output, true, first, last, 
	                			obj.cpvar.length, FZType.V_ARRAY_SET)
	                	if(output) {
	                		first = false
	                	}
	                	output = false
	                	last = false
	                }
	                output = false
	              }
	              case _ => {
	                //System.err.println("The type " + tp.toString() + " is not supported/relevant for the solver")
	              }
	            }  
	        }
	      }
	  } 
	  cp.silent = !options.verbose 
	  
	  /*
	   * need to catch exceptions when objective is not bound
	   */
	  tp match {
	    case "sat" => {
	      cp.solve subjectTo {
	      } exploration {
	        explo(ann, x, s)
	        sol_found = true
	        format_output(xs, xsstate)
	      } run {
	        if (options.all) Int.MaxValue 
	        else if (options.nSolutions > 0) options.nSolutions 
	        else 1
	      }
	    }
	    case "max" => {
	      cp.maximize(
	          expr match {
		        case x:List[Any] =>
		          getCPVarIntFromList(x)
		        case x:String => 
		          getCPVarIntFromString(x)
	          }
	      ) subjectTo {
	      } exploration {
	        explo(ann, x, s)
	        sol_found = true
	        format_output(xs, xsstate)
	      } run ()
	    }
	    case "min" => {
	      cp.minimize(
	          expr match {
		        case x:List[Any] =>
		          getCPVarIntFromList(x)
		        case x:String => 
		          getCPVarIntFromString(x)
	          }
	      ) subjectTo {
	      } exploration {
	        explo(ann, x, s)
	        sol_found = true
	        format_output(xs, xsstate)
	      } run ()
	    }
	  }
	  if(sol_found) {
	    println("==========")
	  } else {
	    println("=====UNSATISFIABLE=====")
	  }
      if (options.statistics) {
        cp.printStats()
      }
	}
	
	
	// should be used to factorize the code in solver(), thx to the different types of fzobjects
//	def getVariable(tp: FZType, fzo: FZObject, xs: (Array[CPVarInt], Array[VarState])): 
//		(Array[CPVarInt], Array[VarState]) = {
//	  var output = false
//	  val obj = fzo.asInstanceOf[FZVarObject]
//	  val a = xs._1 :+ obj.cpvar.asInstanceOf[CPVarInt]
//	  obj.annotations.foreach { ann =>
//	  	if ( ann.name == "output_var" ) { output = true }
//	  }
//	  val s = xs._2 :+ new VarState(obj.name,
//	        output, false, false, false, 1, tp)
//	  (a, s)
//	}
	
	/**
	 * Matches on the search annotation to launch the search on the right variables with the right parameters
	 * @param ann : the list of annotation for the search
	 * @param x : the list of all CPVarInt and CPVarBool in the model
	 * @param s : the list of all CPVarSet in the model
	 * @return Unit
	 */
	def explo(ann: List[Annotation], x: Array[CPVarInt], s: Array[CPVarSet]): Unit @suspendable = {
		if(ann.isEmpty) {
          cp.binary(x)
        }
        else {
          for(a <- ann.suspendable) {
        	a.name match {
		      case "int_search" =>
		        val array = getCPVarIntArray(a.args(0))
		        varChoiceAnn2(a.args, array)
		      case "bool_search" =>
		        val array = getCPVarBoolArray(a.args(0)).map(_.asInstanceOf[CPVarInt])
		        varChoiceAnn2(a.args, array)
	//	      case "set_search" =>
			}
          }
          /*
           * PARTICULAR_SOLUTIONS == false  to get the admissible domains for the variable in the solution
           * PARTICULAR_SOLUTIONS == true  to have a particular solution
           * if false, the output won't necessarily be readable for the formatting tool of minizinc
           */
          if(PARTICULAR_SOLUTIONS) {
        	  cp.binary(x)
          }
        }
		if(!s.isEmpty) {
          for(e <- s.toList.suspendable){
            cp.binary(e)
          }
        }
	}
	
	/**
	 * Matches the variable heuristic to use on the variables in array
	 * @param args : the argument of the search annotations
	 * @param array : an array containing the variable related to the search annotation
	 * @return Unit
	 */
	def varChoiceAnn2(args: List[Any], array: Array[CPVarInt]): Unit @suspendable = {
		args(1) match {
	      case "input_order" => cp.binaryStaticOrder(array, assignAnn(args))//assignAnn(args, array, array.indexOf(_))
	      case "first_fail" => cp.binary(array, _.size,assignAnn(args))
	      case "anti_first_fail" => cp.binary(array, -_.size,assignAnn(args))
	      case "smallest" => cp.binary(array, _.min,assignAnn(args))
	      case "largest" => cp.binary(array, -_.max,assignAnn(args))
	      case "occurence" => cp.binary(array, -_.constraintDegree,assignAnn(args))
	      case "most_constrained" => {
	        System.err.println(args(1) + " not suppported so far occurence instead")
	        cp.binary(array, -_.constraintDegree,assignAnn(args))
	      }
	      case "max_regret" => cp.binary(array, -_.regret,assignAnn(args))
	    }
	}
	
	/**
	 * Matches the value heuristic to use on the variables in array
	 * @param args : the argument of the search annotations
	 * @param array : an array containing the variable related to the search annotation
	 * @return Unit
	 */
	def assignAnn(args: List[Any]): (CPVarInt) => Int  = {		
		args(2) match {
		  case "indomain_min" => _.min
		  case "indomain_max" => _.max
		  case "indomain_middle" => {
			  System.err.println(args(2) + " not suppported so far, in_domain_median used instead")
			  _.median
		  }
		  case "indomain_median" => _.median
		  case "indomain" => {
			  System.err.println(args(2) + " not suppported so far, in_domain_min used instead")
			  _.min
		  }
		  case "indomain_random" => _.randomValue
		  case "indomain_split" => {
			  System.err.println(args(2) + " not suppported so far, in_domain_min used instead")
			  _.min
		  }		  		//should use binary domain split... need to check that the bound used on the intervals (with binarySplit the same as in the spec
		  case "indomain_reverse_split" => {
			  System.err.println(args(2) + " not suppported so far, in_domain_min used instead")
			  _.min
		  }
		  case "indomain_interval" => {
			  System.err.println(args(2) + " not suppported so far, in_domain_min used instead")
			  _.min
		  }
		}
	}
	
	/**
	 * Print the output according to the mzn spec
	 * @param x : array containing all CPVarInt, CPVarBool and CPVarSet in the model
	 * @param state : array of VarState containing information on the CPVarInt, CPVarBool and CPVarSet
	 */
	def format_output(x: Array[CPVar], state: Array[VarState]) {
	  def printCPVar(cpvar: CPVar) {
	    if(cpvar.isInstanceOf[CPVarSet]) {
    	  printSet(cpvar.asInstanceOf[CPVarSet])
    	} else {
    	  print(cpvar.toString)
    	}
	  }
	  
	  for(i <- 0 until x.length) {
    	if ( state(i).output ) { 
    	  if ( state(i).array ) {
    	    if (state(i).first) {
    	    	val ann = getCPArrayOutputAnnotations(state(i).name)
    	    	print(state(i).name + " = array" + ann.length + "d(")
    	    	for(a <- ann) {
    	    	  print(a.min + ".." + a.max + ", ")
    	    	}
    	    	print("[")
    	    	printCPVar(x(i))
    	    	if( state(i).last ) {
	    	    	println("]);")
	    	    }
    	    } else if( state(i).last ) {
    	    	print(", ")
    	    	printCPVar(x(i))
    	    	println("]);")
    	    } else {
    	    	print(", ")
    	    	printCPVar(x(i))
    	    }
    	  } else {
    	    print(state(i).name + " = ")
    	    printCPVar(x(i))
    	    println(";")
    	  }
    	}
	  }
	  println("----------")
	}
	
	/**
	 * Print a set according to the minizinc spec
	 * @param cpset : a CPVarSet
	 */
	def printSet(cpset: CPVarSet) {
		val set = cpset.requiredValues.toSeq.sorted
	    var r2 = 0
	    var r = false
	    var pred = 0
	    if (set.isEmpty) {
	      print("{}")
	    } else {
	      for (v <- set) { 
		      if (v == set.head) {
		        print("{" + v)
		        if(v == set.last) {
		          print("}")
		        }
		      }
		      else if (v != set.last) {
		        if(pred == v-1) {
		          if(!r) { r = true } 
		          r2 = v
		        } else {
		          if(r) {
		            print(".." + r2 + ", " + v)
		            r = false
		          } else {
	    	        print(", " + v)
		          }
		        }
		      } else {
		        if(pred == v-1) {
		          print(".." + v + "}")
		        } else {
		          if(r) {
		            print(".." + r2 + ", " + v + "}")
		          } else {
		            print(", " + v + "}")
		          }
		        }
		      }
		      pred = v
		    }
	    } 
	}
	
	def annotations : Parser[List[Annotation]] = rep("::"~>annotation) 
	// list of annotations : in flatzinc spec pg10
	def annotation : Parser[Annotation] = (
	    pred_ann_id~"("~rep1sep(expr, ",")~")" ^^ {
	      case ann~"("~list~")" => new Annotation(ann, list)
	    }
	    | pred_ann_id ^^ (new Annotation(_, null))
	)
	
}
