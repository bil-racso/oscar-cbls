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
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import oscar.flatzinc.parser.Model
import java.io.PrintStream
import oscar.flatzinc.parser.VarRef
import oscar.flatzinc.parser.intermediatemodel.Element
import oscar.flatzinc.parser.intermediatemodel.ArrayOfElement
import scala.collection.JavaConverters._
import java.util.ArrayList

object FlatZincPrinter {

  def outputModelTo(model:Model, out:PrintStream){
    val dico = model.dico
    //No Parameters (all inlined) TODO
    //Variables
    for((id,e) <- dico.filter(_._2.isInstanceOf[VarRef])){
      //TODO: Make sure that aliases come after the corresponding variable... WHICH ALIASES?
      //TODO: WHERE are the arrays?
      val v = e.asInstanceOf[VarRef].v//.asInstanceOf[ConcreteVariable]
      v match{
        case IntegerVariable(i,d) => out.println("var "+toFZN(d)+": "+ id +" "+toFZNann(model.dicoAnnot(id))+" ;");
        case BooleanVariable(i,v) => out.println("var bool: "+ id +" "+toFZNann(model.dicoAnnot(id))+{if(v.isDefined) "= "+v.get.toString() else ""}+" ;");
      }
    }
    for((id,e) <- dico.filter(_._2.isInstanceOf[ArrayOfElement])){
      if(e.typ.isVar){
        println(id+" "+e)
        val a = e.asInstanceOf[ArrayOfElement].elements
        out.println("array[1.."+a.size()+"] of var int: "+id+toFZNann(model.dicoAnnot(id))+"= "+toFZN(a)+";")
        println("array[1.."+a.size()+"] of var int: "+id+toFZNann(model.dicoAnnot(id))+"= "+toFZN(a)+";")
      }
    }
    for(c <- model.problem.constraints){
      out.println("constraint "+toFZN(c)+";")
    }
    out.println(toFZN(model.problem.search))
  }
  
  def toFZN(s: Search): String = {
    "solve "+toFZNann(s.anns)+" "+{
      s.obj match{
        case Objective.SATISFY => "satisfy"
        case Objective.MAXIMIZE => "maximize "+s.variable.get.id
        case Objective.MINIMIZE => "minimize "+s.variable.get.id
      }
    }+";"
  }
  //TODO: Element should stay in the intermediate model. Annotations should be transformed before that.
  def toFZN(e: Element): String = {
    if(e.value!=null){
      toFZN(e.value)
    }else{
      e.name
    }
  }
  def toFZN(d: Domain): String = {
    d match{
      case DomainRange(min,max) => min+".."+max
      case DomainSet(set) => set.mkString("{", ", ", "}")
    }
  }
  def toFZN(ann: Annotation): String = {
    ann.name+{if(!ann.args.isEmpty) ann.args.map(toFZN(_)).mkString("(", ", ", ")") else ""}
  }
  def toFZN(anns: List[Annotation]): String = {
    anns.map(toFZN(_)).mkString(" ")
  }
  def toFZNann(anns: List[Annotation]): String = {
    if(anns.isEmpty) "" else
    anns.map(toFZN(_)).mkString(":: "," :: ","")
  }
  def toFZN(vs: Array[Variable]): String = {
    vs.map(toFZN(_)).mkString("[",",","]")
  }
  
  def toFZN(a: Any): String = {
    a match{
      case d: Domain => toFZN(d)
      case ann: Annotation => toFZN(ann)
      case anns: List[Annotation] => toFZN(anns)
      case s:String => s
      case c:Constraint => toFZN(c)
      case v:Variable => v.id
      case es: ArrayOfElement => es.elements.asScala.map(toFZN(_)).mkString("[",", ","]")
      case vs: Array[Variable] => toFZN(vs)
      case vs: ArrayList[VarRef] => vs.asScala.map(toFZN(_)).mkString("[",", ","]")
      case e: Element => toFZN(e)
      case x => Console.err.println(x.getClass()); x.toString()
      }
  }
  
  def toFZN(c: Constraint): String = {
    c match{
      case reif(int_eq(x,y,ann),z) => "int_eq_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(int_le(x,y,ann),z) => "int_le_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(set_in(x,y,ann),z) => "set_in_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_eq(x,y,z,ann) => "int_lin_eq("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_le(x,y,z,ann) => "int_lin_le("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_ne(x,y,z,ann) => "int_lin_ne("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_int_element(x,y,z,ann) => "array_int_element("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_bool_and(x,y, ann) => "array_bool_and("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case array_bool_or(x,y, ann) => "array_bool_or("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case bool_clause(x,y, ann) => "bool_clause("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case reif(int_lin_ne(x,y,z,ann),w) => "int_lin_ne_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case reif(int_lin_le(x,y,z,ann),w) => "int_lin_le_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case all_different_int(x,ann) => "all_different_int("+toFZN(x)+")"+toFZNann(ann)
      case GeneratedConstraint(name,args,signature) => name+"("+args.map(toFZN(_)).mkString(", ")+")"
    }
  }
}