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
  ******************************************************************************
  * @author Jean-Noël Monette
  */
grammar Flatzinc;

@header{
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
package oscar.flatzinc.parser;
import oscar.flatzinc.parser.intermediatemodel.*;
import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;
import oscar.flatzinc.model.DomainSet;
import oscar.flatzinc.model.DomainRange;
import oscar.flatzinc.ParsingException;
import java.util.Set;
import java.util.HashSet;

}

@parser::members{
	private Model m;
	public FlatzincParser(TokenStream input,Model m){
		this(input);
		this.m = m;
	}
}

//flatzinc_model : preddecl* paramdecl* vardecl* constraint* solvegoal;
//changed to accomodate Mzn 2.0
flatzinc_model : preddecl* (paramdecl|vardecl)* constraint* solvegoal;

//Nothing is done for the predicate declarations
preddecl : 'predicate' PREDANNID '(' predparam ( ',' predparam)* ')' ';' ;

predparam : predparamtype ':' predannid ;


paramdecl : partype ':' varparid '=' expr ';' 
	{Element e = $expr.e;
	e.name = $varparid.text;
	e.typ = $partype.t;
	m.addId(e.name,e);
	//TODO: Check that expr is a boolconst, floatconst, intconst, setconst, or an array thereof.
	};
// expr must be a boolconst, floatconst, intconst, setconst, or an array thereof.



vardecl locals [Element e = null]
	: vartype ':' varparid annotations ( '=' expr {$e = $expr.e;})? ';'
	{if($e==null){
	  m.addNewVariable($vartype.t,$vartype.d,$varparid.text, $annotations.anns);
	}else{
	  m.addAliasVariable($vartype.t,$vartype.d,$varparid.text, $e, $annotations.anns);
	}
	// TODO: Check that Any vars in assignments must be declared earlier.
	};



constraint locals [List<Element> args = null;]: 'constraint' predannid 
	'(' e=expr {$args = new ArrayList<Element>(); $args.add($e.e);}(',' e1=expr {$args.add($e1.e);})* ')' 
	annotations {m.addConstraint($predannid.text,$args,$annotations.anns);} ';' ;


solvegoal 
	: 'solve' annotations 'satisfy' ';' {m.setSATObjective($annotations.anns);} 
	| 'solve' annotations 'minimize' expr ';' {m.setMINObjective($expr.e,$annotations.anns);}
	| 'solve' annotations 'maximize' expr ';' {m.setMAXObjective($expr.e,$annotations.anns);}
	;
//expr must be a var name or var array element.





//TYPES
predparamtype : parpredparamtype | varpredparamtype;

basicpartype returns [Type t]
	: 'bool' {$t = new Type(Type.BOOL);}
	| 'float' {$t = new Type(Type.FLOAT);}
	| 'int' {$t = new Type(Type.INT);}
	| 'set' 'of' 'int' {$t = new Type(Type.SET);}
	;
	
partype returns [Type t]
	: basicpartype {$t = $basicpartype.t;}
	| arraytype basicpartype {$t = $basicpartype.t; ($t).isArray=true; ($t).size = $arraytype.size;}
	;
	
parpredparamtype returns [Type t]
	: partype {$t = $partype.t;}
	| basicparpredparamtype  {$t = $basicparpredparamtype.t;}
	| arraytype basicparpredparamtype {$t = $basicparpredparamtype.t; ($t).isArray=true; ($t).size = $arraytype.size;}
	;
	
basicparpredparamtype returns [Type t]
	: floatconst '..' floatconst {$t = new Type(Type.FLOAT);}
	| setconst {$t = new Type(Type.INT);}
	| 'set' 'of' setconst {$t = new Type(Type.SET);}
	;
	
vartype returns [Type t, Element d]
	: basicvartype {$t = $basicvartype.t; $d = $basicvartype.d;}
	| arraytype basicvartype {$t = $basicvartype.t; ($t).isArray=true; ($t).size = $arraytype.size; $d = $basicvartype.d;}
	;
	
basicvartype returns [Type t, Element d]
	: 'var' 'bool' {$t = new Type(Type.BOOL); ($t).isVar = true;}
	| 'var' 'float' {$t = new Type(Type.FLOAT); ($t).isVar = true;}
	| 'var' floatconst '..' floatconst {$t = new Type(Type.FLOAT); ($t).isVar = true;} 
 	| 'var' 'int' {$t = new Type(Type.INT); ($t).isVar = true;}
 	| 'var' setconst {$t = new Type(Type.INT); ($t).isVar = true; $d = $setconst.e;}
 	| 'var' 'set' 'of' setconst {$t = new Type(Type.SET); ($t).isVar = true; $d = $setconst.e;}
 	;

varpredparamtype returns [Type t] 
	: vartype {$t = $vartype.t;}
	| 'var' 'set' 'of' 'int' {$t = new Type(Type.SET); ($t).isVar = true;}
	| arraytype 'var' 'set' 'of' 'int' {$t = new Type(Type.SET); ($t).isVar = true; ($t).isArray=true; ($t).size = $arraytype.size;}
	;
	
arraytype returns [int size]:  'array' '[' ( lb=intconst '..' ub=intconst {$size = $ub.i; if($lb.i!=1) throw new ParsingException("Ranges of array must start at 1");} 
  | 'int' {$size = -1;}
  | 'int' ',' 'int' {$size = -1;}) ']' 'of' 
  ;
// TODO: check: "int" and "int,int" are only allowed in predicate declarations.
 


expr returns [Element e]
	: Boolconst {$e = new Element(); ($e).value = $Boolconst.getText().equals("true"); ($e).typ = new Type(Type.BOOL);} 
	| Floatconst {$e = new Element(); ($e).value = Float.parseFloat($Floatconst.getText()); ($e).typ = new Type(Type.FLOAT);}
	| intconst {$e = new Element(); ($e).value = $intconst.i; ($e).typ = new Type(Type.INT);}
	| setconst {$e = $setconst.e; ($e).typ = new Type(Type.SET);} 
	| varparid {$e = m.findId($varparid.text); }
	| varparid '[' intconst ']' {$e = ((ArrayOfElement)m.findId($varparid.text)).elements.get($intconst.i-1); }
	| arrayexpr {$e = $arrayexpr.a;}
	| annotation {$e = new Element(); ($e).value = $annotation.ann; ($e).typ = new Type(Type.ANNOTATION); // TODO: Check this: Annotation and string expressions are only permitted in annotation arguments. 
	} 
	| stringconstant {$e = new Element(); ($e).value = $stringconstant.str; ($e).typ = new Type(Type.STRING);// TODO: Check this: Annotation and string expressions are only permitted in annotation arguments. 
	}
	;
// TODO: Check this: Annotation and string expressions are only permitted in annotation arguments.


//TODO: Why is it called "Domain"?
setconst returns [Element e] locals [Set<Integer> s]
	: lb=intconst '..' ub=intconst {$e = new Element(); ($e).value = new DomainRange($lb.i,$ub.i); }
	| '{' {$s = new HashSet<Integer>();} (f=intconst { $s.add($f.i); } (',' n=intconst { $s.add($n.i);})*)? '}'  {$e = new Element(); ($e).value =m.createDomainSet($s);}
	;

arrayexpr returns [ArrayOfElement a]:
	'[' {$a = new ArrayOfElement(); ($a).typ = new Type(Type.NULL); ($a).typ.isArray = true; $a.typ.size = 0;} 
		(e=expr {$a.elements.add($e.e); $a.typ.size+=1; if($e.e.typ.isVar)$a.typ.isVar = true; } 
		(',' e=expr {$a.elements.add($e.e); $a.typ.size+=1; if($e.e.typ.isVar)$a.typ.isVar = true; } )* )? ']' {$a.close();};

annotations returns [ArrayList<Annotation> anns]
	: {$anns = new ArrayList<Annotation>();} ( '::' annotation {$anns.add($annotation.ann);} )* ;

annotation returns [Annotation ann] 
	: predannid {$ann = new Annotation($predannid.text);} 
	| predannid {$ann = new Annotation($predannid.text);} '(' expr {($ann).add($expr.e);} (',' expr {($ann).add($expr.e);} )* ')' 
	;
// Whether an identifier is an annotation or a variable name can be identified from its type.
// FlatZinc does not permit overloading of names

predannid returns [String text]: PREDANNID {$text=$PREDANNID.getText();};
boolconst : Boolconst;
floatconst : Floatconst;
intconst returns [int i]: INT {$i = Integer.parseInt($INT.getText());};
stringconstant returns [String str]: STRING {$str = $STRING.getText().substring(1,$STRING.getText().length()-1);};
varparid returns [String text]:  VARPARID {$text=$VARPARID.getText();}|PREDANNID {$text=$PREDANNID.getText();} ;

//LEXER rules
Boolconst : 'true' | 'false' ;

PREDANNID : ('a'..'z'|'A'..'Z')('a'..'z'|'A'..'Z'|'0'..'9'|'_')* ;
VARPARID : '_'+ PREDANNID ;



Floatconst : INT '.' NUM (('e'|'E') INT )? | INT ('e'|'E') INT;

INT : ('+' | '-')? NUM ;
fragment NUM : ('0'..'9')+;

STRING :  '"' ~('"')+ '"' ;
WS : (' ' | '\t' | '\n' |'\r\n' ) {skip();} ;
