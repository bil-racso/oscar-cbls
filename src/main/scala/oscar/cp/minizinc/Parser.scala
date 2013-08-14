package oscar.cp.minizinc

import scala.util.parsing.combinator._
import FZType._
import oscar.cp.modeling.CPSolver
import oscar.cp.core._
import oscar.cp.modeling._
import java.io.FileReader
import oscar.cbls.invariants.lib.set.Cardinality
import scala.Equals
import oscar.cp.constraints.EqReifVar
import oscar.cp.constraints.GrEqVarReif
import oscar.cp.constraints.DiffReifVar
import oscar.cp.constraints.EqReif
import oscar.cp.constraints.GrEqCteReif
import oscar.cp.constraints.DiffReif

class Parser extends JavaTokenParsers {// RegexParsers {
	var model : Minizinc_model = new Minizinc_model
	val cp = CPSolver()
	cp.silent = true
	
	//def myParseAll(input: String) = {parseAll(var_decl, input)}
	def myParseAll(input: FileReader) = {parseAll(flatzinc_model, input)}
	def myParseAll(input: String) = {parseAll(flatzinc_model, input)}
	//def myParseAll(input: String) = {parseAll(constraint, input)}
	def parseParam(input: String) = {parseAll(param_decl, input)}
	def parseVar(input: String) = {parseAll(var_decl, input)}
	
	def flatzinc_model : Parser[Any] = rep(pred_decl)~rep(param_decl)~rep(var_decl)~rep(constraint)~solve_goal
	def pred_decl : Parser[Any] = "predicate"~identifier~"("~rep1sep(pred_param, ",")~");" ^^ {
	  case "predicate"~id~"("~parList~");" => //println("predicate " + id)
	  case _ => //println("error in predicate")
	}

	def identifier : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	def pred_param : Parser[Any] = pred_param_type~":"~pred_ann_id // what about no space before the ":" and one after ?
	def pred_param_type : Parser[Any] = par_pred_param_type | var_pred_param_type	
	
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
	def index_set : Parser[Any] = (
	    "1.."~int_const ^^ {
	      case "1.."~i => Range(1, i+1, 1)
	    }
	    | "int" ^^ (_.toString())// what about the fact that "int" is only allowed in predicates ?
	)
	
	def expr : Parser[Any] = ( //need to find a way to return something else than any
		bool_const
		| set_const //should be float -> int -> set, inverted for set to work, need testing
		| float_const
		| int_const
		| var_par_id~"["~int_const~"]" ^^ {
		  case id~"["~i~"]" => List(id, i)
		}
		| var_par_id
		| array_expr
		| annotation
		| "...string constant..." //???
	)
	def pred_ann_id : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	def var_par_id : Parser[String] = "-*[A-Za-z][A-Za-z0-9_]*".r
	
	
	//definition of the constants
	
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
	def int_const : Parser[Int] = "[+-]?[0-9][0-9]*".r ^^ (_.toInt)// [+-] at the begining of the regex in the grammar, what does that mean ?
	def set_const : Parser[Any] = ( 
	    int_const~".."~int_const ^^ { 
	    	case int1~".."~int2 => Range(int1, int2+1, 1) 
	    } 
	    | "{"~>rep1sep(int_const, ",")<~"}" 
	)
	
	def array_expr : Parser[List[Any]] = (
	    //"[]" | not useful since repsep is used instead of rep1sep
	    "["~>repsep(expr, ",")<~"]"
	)
	
	
	//Parameter and variable declarations
	
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
	            new ParamFloat(e.toString.toFloat, id)))) // ! no floats in oscar, what to do ?
	      case "set of int" => model.dict += 
	        ((id, (FZType.P_SET_INT,
	          e match {
	          	case x:Range => new ParamSetOfInt(e, true, id)
	          	case x:List[Any] => new ParamSetOfInt(e, false, id)
	          }
	        )))
	        
	      //code duplication can be easily avoided but what's better 
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
	  //the expr has restriction... what about it ?
	
	def var_decl : Parser[Any] = var_type~":"~var_par_id~annotations~opt("="~expr)~";" ^^
	{ 
	  
	  case tp~":"~id~ann~Some("="~e)~";" => 
	    //include a match on tp to know if array or not
	    e match {
	      case x:Int => model.dict += 
	        ((id, (FZType.V_INT, 
	            new VarInt(ann, CPVarInt(cp, x), id))))
	      case _ => println(tp + " not yet supported when followed by an assignment")
	    }
	  case tp~":"~id~ann~None~";" => 
	    tp match {
	      case "var bool" => 
	        //println("varbool")
	        model.dict +=
	        ((id, (FZType.V_BOOL,
	            new VarBool(ann, CPVarBool(cp), id))))
	      case "array ["~iset~"] of var bool" => model.dict +=
	        ((id, (FZType.V_ARRAY_BOOL, 
	            new VarArrayBool(ann,
	                iset match {
	                	case x:Range => 
	                	  Array.fill(x.length){CPVarBool(cp)} 
	                	case _ => null
	            	}
	            	, id))))
	      case "var int" => model.dict += 
	      	((id, (FZType.V_INT, 
	      		new VarInt(ann, CPVarInt(cp, -10000, 10000), id)))) // what should I do when no assign ?
	      case "var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_INT_RANGE,
	            new VarIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann, 
	                CPVarInt(cp, i1.toString.toInt to i2.toString.toInt), id))))
	            //println("cpvar created")    		
	      case "var set of"~i1~".."~i2 => 
	        //var set of int_const..int_const means a setof ranges ...? same as array so what does that mean ?
//	        model.dict +=
//	        ((id, (FZType.V_SET_INT_R, 
//	            new VarSetIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann,
//	                ))))
	        
	        println(id + " " + i1 + " " + i2)
	      case "var set of"~"{"~intList~"}" => println(id + " " + intList.toString)
	      
	      case "array ["~iset~"] of var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_ARRAY_INT_R, 
	            new VarArrayIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann,
	                iset match {
	                	case x:Range => 
	                	  Array.fill(x.length){CPVarInt(cp, i1.toString.toInt to i2.toString.toInt)} 
	                	case _ => null
	            	}
	            	, id))))
	      case "array ["~iset~"] of var int" => model.dict +=
	        ((id, (FZType.V_ARRAY_INT,
	            new VarArrayInt(ann, 
	            	iset match {
	                	case x:Range => 
	                	  Array.fill(x.length){CPVarInt(cp, -10000, 10000)} 
	                	case _ => null
	            	}
	                , id))))
	        //println(model.dict.toString)
	      //case "array ["~iset~"] of var bool" =>
	    }
	  //case tp~":"~id~";" => println(tp.toString)
	}// the vars in assignment must be declared earlier
	
	
	// Constraint declaration (every constraint should be a case below the match)
	
	def constraint : Parser[Any] = "constraint"~pred_ann_id~"("~rep1sep(expr, ",")~")"~annotations~";" ^^ {
	  case "constraint"~cstr~"("~varList~")"~ann~";" => cstr match {
	    case "bool_eq" =>
	      bool_cstr(varList, ann, cstr)
	    case "bool_le" =>
	      bool_cstr(varList, ann, cstr)
	    case "bool_lt" =>
	      bool_cstr(varList, ann, cstr)
	    case "bool_not" =>
	      bool_cstr(varList, ann, cstr)
	      
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
	      cp.add(maximum(CPArray, getCPVarInt(varList(2))))
	    case "int_min" =>
	      val CPArray = Array[CPVarInt](getCPVarInt(varList(0)), getCPVarInt(varList(1)))
	      cp.add(minimum(CPArray, getCPVarInt(varList(2))))
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
	      
	    // global constraints defined in minizinc/mznlib/
	    case "oscar_alldiff" =>
	      cp.add(allDifferent(getCPVarIntArray(varList(0).toString)))
	    case "alldiff_0" =>
	    case "all_disjoint" =>
	    case "all_equal_int" =>
	    case "among" =>
	    case "at_least_int" => 
	    case "at most_int" =>
	    case "at_most1" =>
	    case "oscar_bin_packing" => 
	      bin_packing(varList, "def")
	    case "oscar_bin_packing_capa" => 	
	      bin_packing(varList, "capa")
	    case "oscar_bin_packing_load" =>
	      bin_packing(varList, "load")
	    case "circuit" => 
	      cp.add(circuit(getCPVarIntArray(varList(0).toString)))
	    case "count_eq" =>
	    case "count_geq" =>
	    case "count_gt" =>
	    case "count_leq" =>
	    case "count_lt" =>
	    case "count_neq" =>
	    case "cumulative" =>
	    case "decreasing_int" =>
	    case "diffn" =>
	    case "disjoint" =>
	    case "distribute" =>
	    case "oscar_element_bool" =>
	      //cp.add(elementVar(getCPVarIntArray(varList(1).toString), getCPVarInt(varList(0)), getCPVarInt(varList(2))))
	    case "oscar_element_int" =>
	      cp.add(elementVar(getCPVarIntArray(varList(1).toString), getCPVarInt(varList(0)), getCPVarInt(varList(2))))
	    case "exactly_int" =>
	    case "global_cardinality" =>
	    case "global_cardinality_closed" =>
	    case "global_cardinality_low_up" => 
	    case "global_cardinality_low_up_closed" =>
	    case "increasing_int" =>
	    case "int_set_channel" =>
	    case "inverse" =>
	    case "inverse_set" =>
	    case "lex_greater_int" => //not used, done with lex_less
	    case "lex_greatereq_int" => //not used, done with lex_lesseq
	    case "oscar_lex_less_int" =>
	      val t1 = getCPVarIntArray(varList(0).toString)
	      val t2 = getCPVarIntArray(varList(1).toString)
	      cp.add(lexLeq(t1, t2))
	      diff_array_cstr(t1, t2)
	    case "oscar_lex_lesseq_int" =>
	      cp.add(lexLeq(getCPVarIntArray(varList(0).toString), getCPVarIntArray(varList(1).toString)))
	    case "oscar_lex2" => //2D -> 1D done, need to parse the constraint
	      lex2_cstr(varList, false)
	    case "link_set_to_booleans" =>
	    case "oscar_maximum_int" =>
	      cp.add(maximum(getCPVarIntArray(varList(0).toString), getCPVarInt(varList(1))))
	    case "member_int" =>
	    case "oscar_minimum_int" =>
	      cp.add(minimum(getCPVarIntArray(varList(0).toString), getCPVarInt(varList(1))))
	    case "nvalue" =>
	    case "partition_set" =>
	    case "range" =>
	    case "regular" => //2D -> 1D needed
	    case "roots" =>
	    case "sliding_sum" =>
	    case "sort" =>
	    case "oscar_strict_lex2" =>
	      lex2_cstr(varList, true)
	    case "subcircuit" =>
	    case "sum_pred" =>
	    case "oscar_table_int" => //2D -> 1D needed
	      val CPArray = getCPVarIntArray((varList(0).toString))
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
	      cp.add(table(CPArray, tuples))
	    case "value_precede_int" =>
	    case "value_precede_chain_int" =>
	  }
	}
	
	def bin_packing(varList: List[Any], tp: String) {
	  val l =
	  tp match {
	    case "def" => 
	      val nbBin = getCPArrayRangeSize(varList(1).toString)
	      Array.fill(nbBin){CPVarInt(cp, 0, varList(0).toString.toInt)}
	    case "load" => getCPVarIntArray(varList(0).toString)
	    case "capa" =>
	      var capaCP = Array[CPVarInt]()
	      val capaInt = getIntArray(varList(0).toString)
	      capaInt.foreach { e =>
	        capaCP :+= CPVarInt(cp, 0, e)
	      }
	      capaCP
	  }
	  cp.add(binpacking(getCPVarIntArray(varList(1).toString).map(_-1), 
	          getIntArray(varList(2).toString), l))
	}
	
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
          cp.add(lexLeq(x, y))
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
          cp.add(lexLeq(x, y))
          if(strict) {
            diff_array_cstr(x, y)
          }
          x = Array[CPVarInt]()
          y = Array[CPVarInt]()
        }
      }
	}
	
	def diff_array_cstr(x: Array[CPVarInt], y: Array[CPVarInt]) {
	  cp.add(sum(x) != sum(y))
	}
	
	def bool_cstr(varList: List[Any], ann: Any, cstr: String) {
	  var cpvar = Array[CPVarBool]()
	  varList.foreach{ e =>
	    cpvar :+= getCPVarBool(e)
	  }
	  cstr match {
	    case "bool_eq" => cp.add(cpvar(0) == cpvar(1))
	    case "bool_le" => cp.add(!cpvar(0) || cpvar(1))
	    case "bool_lt" => cp.add(!cpvar(0) && cpvar(1))
	    case "bool_not" => cp.add(!cpvar(0) == cpvar(1))
	  }
	}
	
	def int_cstr(varList: List[Any], ann: Any, cstr: String) {
	  var cpvar = Array[CPVarInt]()
	  varList.foreach{ e => 
	    cpvar :+= getCPVarInt(e)
	  }
	  cstr match {
	    case "int_eq" => cp.add(cpvar(0) == cpvar(1))
	    case "int_le" => cp.add(cpvar(0) <= cpvar(1))
	    case "int_lt" => cp.add(cpvar(0) < cpvar(1))
	    case "int_ne" => cp.add(cpvar(0) != cpvar(1))
	    case "int_plus" => cp.add(cpvar(0) + cpvar(1) == cpvar(2))
	    case "int_times" => cp.add(cpvar(0) * cpvar(1) == cpvar(2))
	  }
	}
	
	def int_reif(varList: List[Any], ann: Any, cstr: String) {
	  var cpvar = Array[CPVarInt]()
	  for(i <- 0 until varList.length-1) {
	    cpvar :+= getCPVarInt(varList(i))
	  }
	  val boolvar = getCPVarBool(varList(varList.length-1))
	  cstr match {
	    case "int_eq_reif" => cp.add(new EqReifVar(cpvar(0), cpvar(1), boolvar))
	    case "int_le_reif" => cp.add(new GrEqVarReif(cpvar(1), cpvar(0), boolvar))
	    case "int_lt_reif" => cp.add(new GrEqVarReif(cpvar(1), cpvar(0)+1, boolvar))
	    case "int_ne_reif" => cp.add(new DiffReifVar(cpvar(0), cpvar(1), boolvar))
	  }
	  
	}
	
	def int_lin_cstr(varList: List[Any], ann: Any, cstr: String) {
	  //assert(varList.length == 3, "To many arguments for int_lin_ne")
      var cpvar = Array[CPVarInt]()
      varList(1) match {
	    case x:List[Any] =>
	      x.asInstanceOf[List[Any]].foreach { e =>
	      	cpvar :+= getCPVarInt(e)
	      }
	    case x:String =>
	      cpvar = getCPVarIntArray(x)
	  }
	  var cst = varList(0) match {
        case x:List[Any] => varList(0).asInstanceOf[List[Int]].toArray
        case x:String => getIntArray(x)
      }
      //val cst: Array[Int] = varList(0).asInstanceOf[List[Int]].toArray
      val c = varList(2) match {
        case x:Int => x
        case x:String => getInt(x)
      }
      cstr match {
        case "int_lin_ne" => 
          cp.add(weightedSum(cst, cpvar) != c)
        case "int_lin_eq" =>
          cp.add(weightedSum(cst, cpvar) == c)
        case "int_lin_le" => 
          cp.add(weightedSum(cst, cpvar) <= c) 
        case "int_lin_eq_reif" => 
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
        case "int_lin_le_reif" =>
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
        case "int_lin_ne_reif" =>
          int_lin_reif_cstr(cpvar, cst, c, varList, ann, cstr)
      }
	}
	
	def int_lin_reif_cstr(cpvar: Array[CPVarInt], cst: Array[Int], c: Int, 
	    varList: List[Any], ann: Any, cstr: String) {
      val boolvar = getCPVarBool(varList(varList.length-1))
      cstr match {
        case "int_lin_eq_reif" => 
          cp.add(new EqReif(weightedSum(cst, cpvar), c, boolvar))
        case "int_lin_le_reif" =>
          cp.add(new GrEqCteReif(-weightedSum(cst, cpvar), -c-1, boolvar))
        case "int_lin_ne_reif" =>
          cp.add(new DiffReif(weightedSum(cst, cpvar), c, boolvar))
      }
	}
	
	def getInt(x:String): Int = {
	  model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.P_INT => {
	              fzo.asInstanceOf[ParamInt].value
	            }
	        }
	    }
	}
	
	def getIntArray(x: String): Array[Int] = {
	  model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.P_ARRAY_INT => {
	              val list = fzo.asInstanceOf[ParamArrayInt].value.asInstanceOf[List[Int]]
	              (list map (_.toInt)).toArray
	            }
	        }
	    }
	}
	
	def getCPVarBool(x: Any): CPVarBool = {
	  x match {
	    case x:List[Any] => getCPVarBoolFromList(x)
	    case x:String => getCPVarBoolFromString(x)
	    case x:Boolean => CPVarBool(cp, x)
	  }
	}
	
	def getCPVarInt(x: Any): CPVarInt = {
	  x match {
	    case x:Int => CPVarInt(cp, x)
	    case x:List[Any] => getCPVarIntFromList(x)
	    case x:String => getCPVarIntFromString(x)
	  }
	}
	
	def getCPVarBoolFromString(x: String): CPVarBool = {
      model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.V_BOOL => {
	              fzo.asInstanceOf[VarBool].cpvar
	            }
	        }
	    }
	}
	
	def getCPVarIntFromString(x: String): CPVarInt = {
	  model.dict.get(x) match {
	      case Some((tp, fzo)) => 
	        tp match {
	            case FZType.V_INT => {
	              fzo.asInstanceOf[VarInt].cpvar
	            }
	            case FZType.V_INT_RANGE => {
	              fzo.asInstanceOf[VarIntRange].cpvar
	            }
	        }
	    }
	}
	
	def getCPVarBoolFromList(x: List[Any]): CPVarBool = {
	  model.dict.get(x(0).toString) match {
          case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_BOOL => {
                  fzo.asInstanceOf[VarArrayBool].cpvar(x(1).toString.toInt-1)
                }
            }
        }
	}
	
	def getCPVarIntFromList(x: List[Any]): CPVarInt = {
	  model.dict.get(x(0).toString) match {
          case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_INT_R => {
                  fzo.asInstanceOf[VarArrayIntRange].cpvar(x(1).toString.toInt-1)
                }
                case FZType.V_ARRAY_INT => {
                  fzo.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
                }
            }
        }
	}
	
	def getCPVarIntArray(x: String): Array[CPVarInt] = {
	  model.dict.get(x) match {
		  case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_INT_R => {
                  fzo.asInstanceOf[VarArrayIntRange].cpvar
                }
                case FZType.V_ARRAY_INT => {
                  fzo.asInstanceOf[VarArrayInt].cpvar
                }
            }
	  }
	}
	
	def getCPArrayRangeSize(x: String): Int = {
	  model.dict.get(x) match {
		  case Some((tp, fzo)) => 
            tp match {
                case FZType.V_ARRAY_INT_R => {
                  fzo.asInstanceOf[VarArrayIntRange].range.length
                }
            }
	  }
	}
	
	def solve_goal : Parser[Any] = (
	    "solve"~annotations~"satisfy;" ^^ { 
	      case "solve"~ann~"satisfy;" => solver("sat", null)
	    }
	    | "solve"~annotations~"minimize"~expr~";"
	    | "solve"~annotations~"maximize"~expr~";" ^^ { 
	      case "solve"~ann~"maximize"~e~";" => solver("max", e)
	    }
	) // expr must be a var name of var array element
	
	def solver(tp: String, expr: Any) {
	  var x = Array[CPVarInt]() //
      var state = Array[VarState]()
      var output: Boolean = false // only used for formating the output
      var c = 0
      model.dict.foreach { e => 
        e._2 match {
          case (tp, fzo) => // /!\ not always CPVarInt
            //println("ici")
            tp match {
              case FZType.V_BOOL => {
                x :+= fzo.asInstanceOf[VarBool].cpvar
                fzo.asInstanceOf[VarBool].annotations.foreach { ann =>
            		if ( ann.name == "output_var" ) { output = true }
                }
                state :+= new VarState(fzo.asInstanceOf[VarBool].name,
                    output, false, false, 1)
                output = false
              }
              case FZType.V_INT_RANGE => {
                //println(fzo.asInstanceOf[VarIntRange].cpvar)
                x :+= fzo.asInstanceOf[VarIntRange].cpvar
                fzo.asInstanceOf[VarIntRange].annotations.foreach { ann =>
            		if ( ann.name == "output_var" ) { output = true }
                }
                state :+= new VarState(fzo.asInstanceOf[VarIntRange].name,
                    output, false, false, 1)
                output = false
                
              }
              case FZType.V_ARRAY_INT_R => {
                //var c = 0
                var first = true
                fzo.asInstanceOf[VarArrayIntRange].cpvar.foreach { e =>
                	//println(e)
                	x :+= e
                	fzo.asInstanceOf[VarArrayIntRange].annotations.foreach { ann =>
            			if ( ann.name == "output_array" ) { output = true }
                	}
                	state :+= new VarState(fzo.asInstanceOf[VarArrayIntRange].name,
                    output, true, first, fzo.asInstanceOf[VarArrayIntRange].cpvar.length)
                	
                	first = false
                }
                output = false
              }
              case FZType.V_INT => {
                x :+= fzo.asInstanceOf[VarInt].cpvar
                fzo.asInstanceOf[VarInt].annotations.foreach { ann =>
            		if ( ann.name == "output_var" ) { output = true }
                }
                state :+= new VarState(fzo.asInstanceOf[VarInt].name,
                    output, false, false, 1)
                output = false
              }
              case FZType.V_ARRAY_INT => {
                var first = true
                fzo.asInstanceOf[VarArrayInt].cpvar.foreach { e =>
                	//println(e)
                	x :+= e
                	fzo.asInstanceOf[VarArrayInt].annotations.foreach { ann =>
            			if ( ann.name == "output_array" ) { output = true }
                	}
                	state :+= new VarState(fzo.asInstanceOf[VarArrayInt].name,
                    output, true, first, fzo.asInstanceOf[VarArrayInt].cpvar.length)
                	
                	first = false
                }
                output = false
              }
              case _ => {
                println("The type " + tp.toString() + " is not supported/relevant for the solver")
              }
            }
            
        }
      }
	  tp match {
	    case "sat" => {
	      cp.solve subjectTo {
	      } exploration {
	        cp.binary(x)
	        format_output(x, state)
	      } run (1)
	      println("==========")
	    }
	    case "max" => {
	      cp.maximize(
	          expr match {
		        //case x:List[List[Any]] => //can oscar max several values,... can it be done in cp ?
		        case x:List[Any] =>
		          getCPVarIntFromList(x)
		        case x:String => 
		          getCPVarIntFromString(x)
	          }
	      ) subjectTo {
	      } exploration {
	        cp.binary(x)
	        format_output(x, state)
	      } run ()
	      println("==========")
	    }
	    case "min" => {
	      cp.minimize(
	          expr match {
		        //case x:List[List[Any]] => //can oscar max several values,... can it be done in cp ?
		        case x:List[Any] =>
		          getCPVarIntFromList(x)
		        case x:String => 
		          getCPVarIntFromString(x)
	          }
	      ) subjectTo {
	      } exploration {
	        cp.binary(x)
	        format_output(x, state)
	      } run ()
	      println("==========")
	    }
	  }   
	}
	
	def format_output(x: Array[CPVarInt], state: Array[VarState]) {
		var c = 0
		Range(0, x.length, 1).foreach { i =>
	    	if ( state(i).output ) { 
	    	  if ( state(i).array ) {
	    	    c += 1
	    	    if (state(i).first) {
	    	    	print(state(i).name + " = array1d(1.." + state(i).size + ", [" + x(i).toString.split(" ")(1))
	    	    } else if( c == state(i).size ) {
	    	    	println("," + x(i).toString + "]);")
	    	    	c = 0
	    	    } else {
	    	    	print("," + x(i).toString)
	    	    }
	    	  } else {
	    	  	println(state(i).name + " =" + x(i).toString + ";") 
	    	  }
	    	}
	    }
	    println("----------")
	}
	
	def annotations : Parser[List[Annotation]] = rep("::"~>annotation) 
	// is there a list of annotations ?
	def annotation : Parser[Annotation] = (
	    pred_ann_id~"("~rep1sep(expr, ",")~")" ^^ {
	      case ann~"("~list~")" => new Annotation(ann, list)
	    }
	    | pred_ann_id ^^ (new Annotation(_, null))
	)// some notes, see syntax
	
}