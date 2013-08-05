package oscar.cp.minizinc

import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers {// RegexParsers {
	def flatzinc_model : Parser[Any] = rep(pred_decl)~rep(param_decl)~rep(var_decl)~rep(constraint)~solve_goal
	def pred_decl : Parser[Any] = "predicate"~identifier~"("~rep1sep(pred_param, ",")~");"

	def identifier : Parser[Any] = index_set
	
	def pred_param : Parser[Any] = pred_param_type~":"~pred_ann_id // what about no space before the ":" and one after ?
	def pred_param_type : Parser[Any] = par_pred_param_type | var_pred_param_type
	def par_type : Parser[Any] = (
	    "bool"
		| "float"
		| "int"
		| "set of int"
		| "array"~opt(index_set)~"of bool"
		| "array"~opt(index_set)~"of float"
		| "array"~opt(index_set)~"of int"
		| "array"~opt(index_set)~"of set of int"
	)
	def par_pred_param_type : Parser[Any] = (
		par_type
		| float_const~".."~float_const
		| int_const~".."~int_const
		| "{"~rep1sep(int_const, ",")~"}"
		| "set of"~int_const~".."~int_const
		| "set of"~"{"~rep1sep(int_const, ",")~"}"
		| "array"~opt(index_set)~"of"~float_const~".."~float_const
		| "array"~opt(index_set)~"of"~int_const~".."~int_const
		| "array"~opt(index_set)~"of"~"{"~rep1sep(int_const, ",")~"}"
		| "array"~opt(index_set)~"of"~"set of"~int_const~".."~int_const
		| "array"~opt(index_set)~"of"~"set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_type : Parser[Any] = (
		"var float"
	    | "var"~float_const~".."~float_const
	    | "var int"
	    | "var"~int_const~".."~int_const
	    | "var"~"{"~rep1sep(int_const, ",")~"}"
	    | "var set of"~int_const~".."~int_const
	    | "var set of"~"{"~rep1sep(int_const, ",")~"}"
	    | "array"~opt(index_set)~"of var bool"
	    | "array"~opt(index_set)~"of var float"
	    | "array"~opt(index_set)~"of var"~float_const~".."~float_const
	    | "array"~opt(index_set)~"of var int"
	    | "array"~opt(index_set)~"of var"~int_const~".."~int_const
	    | "array"~opt(index_set)~"of var"~"{"~rep1sep(int_const, ",")~"}"
	    | "array"~opt(index_set)~"of var set of"~int_const~".."~int_const
	    | "array"~opt(index_set)~"of var set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_pred_param_type : Parser[Any] = (
		var_type
		| "var set of int"
		| "array"~opt(index_set)~"of var set of int"
	)
	def index_set : Parser[Any] = "1.."~int_const | "int" // what about the fact that "int" is only allowed in predicates ?
	def expr : Parser[Any] = (
		bool_const
		| float_const
		| int_const
		| set_const
		| var_par_id
		| var_par_id~opt(int_const) // why two entries for var_par_id ?
		| array_expr
		| annotation
		| "...string constant..." //???
	)
	def pred_ann_id : Parser[Any] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	def var_par_id : Parser[Any] = "-*[A-Za-z][A-Za-z0-9_]*".r
	
	def bool_const : Parser[Boolean] = (
	    "true" ^^ (x => true)
	    | "false" ^^ (x => false)
	)
	
	def float_const : Parser[Double] = (
	    int_const~"."~"[0-9][0-9]*".r~opt("[eE]".r~int_const) ^^ (_.toString.toDouble)
	    | int_const~"[eE]".r~int_const ^^ (_.toString.toDouble)
	)
	
	def int_const : Parser[Int] = "[+-][0-9][0-9.*]".r ^^ (_.toInt)// +- seems to be optional ... or should it be + or -
	
	def set_const : Parser[Any] = int_const~".."~int_const | "{"~rep1sep(int_const, ",")~"}"
	def array_expr : Parser[Any] = "[]" | "["~rep1sep(expr, ",")~"]"
	def param_decl : Parser[Any] = par_type~":"~var_par_id~"="~expr~";" //the expr has restriction... wha about it ?
	def var_decl : Parser[Any] = var_type~":"~var_par_id~annotations~opt("="~expr)~";" // the vars must be declared earlier
	def constraint : Parser[Any] = "constraint"~pred_ann_id~"("~rep1sep(expr, ",")~")"~annotations~";"
	def solve_goal : Parser[Any] = (
	    "solve"~annotations~"satisfy;"
	    | "solve"~annotations~"minimize"~expr~";"
	    | "solve"~annotations~"maximize"~expr~";"
	) // expr must be a var name of var array element
	def annotations : Parser[Any] = rep("::"~annotation)
	def annotation : Parser[Any] = pred_ann_id | pred_ann_id~"("~rep1sep(expr, ",")~")" // some notes, see syntax
	
	

}