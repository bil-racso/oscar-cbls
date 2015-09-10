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
 * *****************************************************************************/

package oscar.cp.xcsp.ast

import util.parsing.combinator._
import oscar.cp.xcsp.XCSPParser

class ParameterParser(variablesInScope : Seq[String]) extends JavaTokenParsers {

  private val variables : Parser[String] = (variablesInScope sortBy(- _.length) reduce {(a,b) => a+"|"+b}).r

  private def effectiveIntegerValue = wholeNumber ^^ (v => EffectiveIntegerValue(v.toInt))
  private def integerVariable = variables ^^ (n => IntegerVariable(n))
  def nil : Parser[EffectiveParameter] = "<nil/>" ^^ { _ => new Nil}
  def effectiveParameter : Parser[EffectiveParameter] = effectiveIntegerValue | integerVariable
  
  def variableList : Parser[List[IntegerVariable]] = "[" ~> rep1(integerVariable) <~ "]"
  def integerList : Parser[List[EffectiveIntegerValue]] = "[" ~> rep1(effectiveIntegerValue) <~ "]"
  def parameterList : Parser[List[EffectiveParameter]] = "[" ~> rep1(effectiveParameter) <~ "]"
  
  def parameterDictionay : Parser[Map[EffectiveIntegerValue,IntegerVariable]] = "[" ~> rep1(parameterEntry) <~ "]" ^^ {l => 
  	val keys = l.map(_._1)
  	val values = l.map(_._2)
  	(keys zip values).toMap
  }
  
  def parameterEntry : Parser[(EffectiveIntegerValue,IntegerVariable)] = "{" ~> effectiveIntegerValue ~ integerVariable  <~ "}" ^^ {case i ~ v => (i,v)}
  
  def relationalOp : Parser[String] = XCSPParser.operators map {f => Parser(f)} reduce {_ | _}

  def weightedSumParameters =  parameterDictionay ~ relationalOp ~  effectiveIntegerValue ^^ { case dic ~ op ~ i => (dic,op,i)}
  def amongParameters = integerVariable ~ variableList ~  integerList ^^ { case v ~ vL ~ iL => (v,vL,iL)}
  def atLeastParameters = effectiveIntegerValue ~ variableList ~  effectiveIntegerValue ^^ { case i1 ~ vL ~ i2 => (i1,vL,i2)}
  def atMostParameters = atLeastParameters
  
  def cumulativeParameters : Parser[(List[(EffectiveParameter,EffectiveParameter,EffectiveParameter,EffectiveParameter)],EffectiveIntegerValue)] = cumulativeTaskList ~ effectiveIntegerValue ^^ {case taskList ~ height => (taskList,height)}
  def cumulativeTaskList = "[" ~> rep1(cumulativeTask) <~ "]"
  def cumulativeTask = "{" ~> (effectiveParameter | nil) ~ (effectiveParameter | nil) ~ (effectiveParameter | nil) ~ effectiveParameter <~ "}" ^^ { 
    case start ~ duration ~ end ~ demand => (start , duration , end , demand)
  }
  
  def disjunctiveParameters : Parser[List[(EffectiveParameter,EffectiveParameter)]] = disjunctiveTaskList
  def disjunctiveTaskList = "[" ~> rep1(disjunctiveTask) <~ "]"
  def disjunctiveTask = "{" ~> effectiveParameter ~ effectiveParameter <~ "}" ^^ { case start ~ duration => (start , duration )}
  
  def elementParameters = integerVariable ~ variableList ~ integerVariable ^^ { case index ~ table ~ value => (index , table, value )}
  
  def globalCardinalityParameters = variableList ~ parameterDictionay ^^ { case variables ~ values => (variables , values)}
  
  def matrixEntry = "{" ~> effectiveIntegerValue ~ effectiveIntegerValue ~ effectiveIntegerValue <~ "}" ^^ {case i ~ j ~ c => (i,j,c)}
  def matrix = "[" ~> rep1(matrixEntry) <~ "]"
  def minimumWeightAllDifferentParameters = variableList ~ matrix ~ integerVariable ^^ { case variables ~ matrix ~ cost => (variables , matrix, cost)}
  
}

/******case classes**************/

class EffectiveParameter
case class EffectiveIntegerValue(value : Int) extends EffectiveParameter
case class IntegerVariable(name : String) extends EffectiveParameter
case class Nil() extends EffectiveParameter

/*********************************/
