package oscar.cp.xcsp.ast

import util.parsing.combinator._

class FunctionalPredicateParser(formalParameterNames : Seq[String]) extends JavaTokenParsers {

  //formal parameters accepted by the considered predicate
  private val formalParameter : Parser[String] = (formalParameterNames sortBy(- _.length) reduce {(a,b) => a+"|"+b}).r
  
  private def integerExpression : Parser[IntegerExpr] = wholeNumber ^^ (v => IntegerValue(v.toInt)) | formalParameter ^^ (n => IntegerFormalParameter(n)) | integerUnaryOperator  | integerBinaryOperator | alternativeOperator
  
  def booleanExpression : Parser[BooleanExpr] = "false" ^^ (_ => BooleanValue(false)) | "true" ^^ (_ => BooleanValue(true)) | notExpression |
  booleanOperatorBooleanParam | booleanOperatorIntegerParams
  
  private def integerUnaryOperator : Parser[IntegerExpr] = ("neg" | "abs") ~ "(" ~ integerExpression ~ ")" ^^ { case name ~ "(" ~ expr ~ ")" => IntUnaryOperator(name,expr)} 
  private def integerBinaryOperator = ("add" | "sub" | "mul" | "div" | "mod" | "pow" | "min" | "max") ~ "(" ~ integerExpression ~ "," ~ integerExpression ~ ")" ^^ { case name ~ "(" ~ left ~ "," ~ right ~ ")" => IntOperatorOnIntegers(name,left,right)}
  
  private def booleanOperatorBooleanParam : Parser[BooleanExpr] = ("and" | "or" | "xor" | "iff") ~ "(" ~ booleanExpression ~ "," ~ booleanExpression ~ ")" ^^ { case name ~ "(" ~ left ~ "," ~ right ~ ")" => BooleanBinaryOperator(name,left,right)}
  private def booleanOperatorIntegerParams : Parser[BooleanExpr] = ("eq" | "ne" | "ge" | "gt" | "le" | "lt") ~ "(" ~ integerExpression ~ "," ~ integerExpression ~ ")" ^^ { case name ~ "(" ~ left ~ "," ~ right ~ ")" => BooleanOperatorOnIntegers(name,left,right)} 
  
  private def alternativeOperator : Parser[IntegerExpr] = "if(" ~ booleanExpression ~ "," ~ integerExpression ~ "," ~ integerExpression ~ ")" ^^ { case "if(" ~ condition ~ "," ~ consequence ~ "," ~ alternative ~ ")" => AlternativeOperator(condition,consequence,alternative)}
  private def notExpression : Parser[BooleanExpr] =  "not(" ~ booleanExpression ~ ")" ^^ {case "not(" ~ expr ~ ")" => Not(expr)}
}

/******case classes**************/

class PredicateExpr
class IntegerExpr extends PredicateExpr
class BooleanExpr extends PredicateExpr

case class IntUnaryOperator(name : String, operand : IntegerExpr) extends IntegerExpr
case class IntOperatorOnIntegers(name : String, leftOperand : IntegerExpr, rightOperand : IntegerExpr) extends IntegerExpr
case class BooleanOperatorOnIntegers(name : String, leftOperand : IntegerExpr, rightOperand : IntegerExpr) extends BooleanExpr
case class BooleanBinaryOperator(name : String, leftOperand : BooleanExpr, rightOperand : BooleanExpr) extends BooleanExpr
case class Not(operand : BooleanExpr) extends BooleanExpr
case class AlternativeOperator(condition : BooleanExpr, consequence : IntegerExpr, alternative : IntegerExpr) extends IntegerExpr

case class IntegerValue(value : Int) extends IntegerExpr
case class BooleanValue(value : Boolean) extends BooleanExpr

case class IntegerFormalParameter(name : String) extends IntegerExpr //only int is available in xcsp

/*********************************/

object FunctionalPredicateParser {
  def parseExpression(formalParameterNames : Seq[String],expression : String) = {
    val parser = new FunctionalPredicateParser(formalParameterNames)
	parser.parseAll(parser.booleanExpression,expression)
  }
}