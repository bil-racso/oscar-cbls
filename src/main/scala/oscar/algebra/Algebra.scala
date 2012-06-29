/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
 ******************************************************************************/
package oscar.algebra


import scala.collection._



/**
 * Abstract trait to manipulate mathematical expression (linear or not) involving symbolic variables
 *  @author Gilles Scouvart (n-Side), Pierre Schaus (n-Side)
 */
trait Algebra {
  
  
  // -------------------------  linear expressions & constraints -------------------

  /**Abstract type for variables*/

  
  abstract class LinearExpression extends Expression {
    
    val cte: Double
    val coef: Map[Var,Double]
    
    def +(expr : LinearExpression) : LinearExpression = new LinearExpressionSum(expr,this)
    
    def -(expr : LinearExpression) : LinearExpression = new LinearExpressionDiff(this,expr)
    
    def unary_- : LinearExpression = new LinearExpressionDiff(0,this)
    
    def <=(linExpr : LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.LQ)
    
    def >=(linExpr : LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.GQ)
     
    def ==(linExpr : LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.EQ)
    
    /**
     * Test if two linear expressions are logically equivalent
     */
    override def equals(that: Any) = {
    	that match { 
    		case other: LinearExpression => {
    		  other.cte == cte && other.coef == coef
    		}
    		case _ => false 
    	}
    }
	 
    def eval(env : Var => Double) : Double = cte + coef.map(e => env(e._1)*e._2).sum
    
    override def value : Option[Double] = {
      var res = cte
      for ((x,a) <- coef) {
        x.value match {
          case None => return None
          case Some(v) => {
            res += a * v
          }
        }
      }
      Some(res)
    }
  }
  
  class Const (val d : Double) extends LinearExpression {
    
    val cte = d
    val coef = Map[Var,Double]()
    
    def *(expr : LinearExpression) : LinearExpression = new LinearExpressionProd(this,expr)

    def *(c2 : Const) = new Const(d*c2.d)
    
    def + (c2: Const) = new Const(d+c2.d)
    
    def - (c2: Const) = new Const(d-c2.d)
    
    def *(x : Var) = new CstVar(this,x)
    
    override def toString = d.toString
    
    def derive(v : Var) : Expression = Zero  
    
  }
  
  object Const {
    def apply(d : Double) : Const = d match {
      case 0.0 => Zero
      case 1.0 => One
      case _ => new Const(d)
    }
    def unapply(c : Const) : Option[Double] = Some(c.d)
  }  
  
  case object Zero extends Const(0) {  
    override def *(expr : LinearExpression) = this
    override def +(expr : LinearExpression) = expr
  }
  
  case object One extends Const(1) {
    override def *(expr : LinearExpression) = expr
  }  
  
  /**Abstract class for variables*/
  abstract class Var extends LinearExpression { 
       
    def name : String
    val ub : Double
    val lb : Double
    val index : Int
    
    val cte = 0.0
    val coef = Map(this->1.0)
    
    override def toString = name

    
    def derive(v : Var) : Expression = {
		if (v == this) One
		else Zero
	}
    
    def *(cons : Const) : LinearExpression = new CstVar(cons,this)
  
    override def equals(that: Any) = {
          	that match { 
    		case other: Var => {
    		  other.index == index
    		}
    		case _ => false 
    	}
    }
    
  }  
   
  /**
   * (expr1 + expr2) or (expr1 - expr2) 
   */
  abstract class LinearExpressionBinary(val expr1: LinearExpression, val expr2: LinearExpression) extends LinearExpression {
   
    val cte = op(expr1.cte,expr2.cte)
    val coef = merge()    
      
    def op(v1: Double, v2: Double): Double
    
    val opStr: String
       

    override def toString() = "("+expr1+ opStr +expr2+")"
    
    
    def merge() : Map[Var,Double] = {
       import scala.collection.mutable.Map
       val mymap = Map[Var,Double]()
       for ((k,v) <- expr1.coef) {
         mymap += k -> v
       }
       for ((k,v) <- expr2.coef) {
         mymap.get(k) match {
          case Some(c) => mymap(k) = op(c,v)
          case None => mymap += (k -> op(0,v))
        }
       }
       mymap.filterNot(_._2 == 0)
    }
   
  
  }  
  
  /**
   * (linExpr1 + linExpr2)
   */
  class LinearExpressionSum(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1,expr2) {
        
    val opStr = "+"

    def op(v1: Double, v2: Double) = v1 + v2
    
    def derive(v : Var) : Expression = {
		expr1.derive(v) + expr2.derive(v)
	}
  }
  
  /**
   * (linExpr1 - linExpr2)
   */
  class LinearExpressionDiff(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1,expr2) {
    val opStr = "-"

    def op(v1: Double, v2: Double) = v1 - v2
    
    def derive(v : Var) : Expression = {
		expr1.derive(v) - expr2.derive(v)
	}
  }  
  
  /**
   * (c * linExpr)
   */
  class LinearExpressionProd(val c: Const, val expr: LinearExpression) extends LinearExpression {
    
    val cte = if (c == Zero) 0.0 else c.d * expr.cte
    val coef = if (c == Zero) Map[Var,Double]() else expr.coef.map(e => (e._1 -> c.d *e._2))   
      

    def derive(v : Var) : Expression = {
		c * expr.derive(v)
	}
    override def toString = c+"*(" + expr + ")"
  
  }  


  /**
   * (c * x)
   */
  class CstVar(val coeff: Const, val variable: Var) extends LinearExpression {

    
    val cte = 0.0
    val coef = if (coeff == 0) Map[Var,Double]() else Map(variable -> coeff.d)

     
    override def toString = coeff + "*" + variable
    
    def derive(v : Var) : Expression = {
		if (variable == v) coeff
		else Zero
	}
   
  }
  
  object CstVar {
    def unapply(l : CstVar) : Option[(Const, Var)] = Some(l.coeff, l.variable)
  }
  
  
  
  def sum(exprs : Iterable[LinearExpression]) : LinearExpression = exprs.foldLeft(Zero : LinearExpression)(_ + _)
  
  def sum[A](indexes : Iterable[A])(f : A => LinearExpression) : LinearExpression = sum(indexes map f)
   
  def sum[A,B](indexes1 : Iterable[A],indexes2 : Iterable[B])(f : (A,B) => LinearExpression) : LinearExpression = {
                        sum(for(i <- indexes1;j <- indexes2) yield f(i,j))
  }

  // ------------   Implicits -----------------------
  
  implicit def double2const(d : Double) : Const = if (d == 0.0) Zero else Const(d)
  implicit def int2const(i : Int) : Const = double2const(i)
  
  // --------------- Linear Constraints -------------
  
  /**
   * A linear constraint has the form (linearExpression REL 0) with REL in {<=, ==, >=}
   */
  class LinearConstraint(val linExpr: LinearExpression, val consType: ConstraintType.Value)
  
  // ------------------------- general mathematical expressions -------------------
  
  abstract class Expression {
    
     def value : Option[Double]
    
     def eval(env : Var => Double) : Double
     
     def +(expr : Expression) : Expression = new Sum(this,expr)
    
     def -(expr : Expression) : Expression = new Diff(this,expr)
     
     def *(expr : Expression) : Expression = new Prod(this,expr)
     
     def /(expr : Expression) : Expression = new Frac(this,expr)
     
     def derive(x: Var): Expression
     
     def isZero(): Boolean = false
  }
  
  abstract class BinaryOp extends Expression {
    val a : Expression
    val b : Expression
    val symb : String
    val op : (Double, Double) => Double
    override def toString = "("+a + symb + b+")"
    
    def eval(env : Var => Double) = {
    	op(a.eval(env), b.eval(env))
    
    }
    
    def value = {
      (a.value,b.value) match {
        case (Some(va),Some(vb)) => Some(op(va,vb))
        case _ => None
      }
    }
  }
  
  case class Sum(val a : Expression, val b : Expression) extends BinaryOp {    
    val symb = "+"
    val op = (a : Double, b : Double) => a + b
    def derive(v : Var) : Expression = {
    	 a.derive(v) + b.derive(v)
    }
    override def isZero() = a.isZero() && b.isZero()
    
  }
  
  case class Diff(val a : Expression, val b : Expression) extends BinaryOp {    
    val symb = "+"
    val op = (a : Double, b : Double) => a - b
    def derive(v : Var) : Expression = {
    	 a.derive(v) + b.derive(v)
    }
    override def isZero() = a.isZero() && b.isZero()
    
  }  
    
  case class Prod(val a : Expression, val b : Expression) extends BinaryOp {
    val symb = "*"
    val op = (a : Double, b : Double) => a * b
    def derive(v : Var) : Expression = {
    	a * b.derive(v) + b * a.derive(v)
    }
    override def toString = "(" + a + ")*(" + b + ")"
    override def isZero() = a.isZero() || b.isZero()
  }
  
  case class Frac(num : Expression, denom : Expression) extends BinaryOp {
	//require(!denom.isZero)
    val a = num
    val b = denom
    val symb = "/"
    val op = (a : Double, b : Double) => a / b
    def derive(v : Var) : Expression = {
    	val fprime = num.derive(v)
        val gprime = denom.derive(v)
        (fprime * denom - gprime * num) / (denom * denom)
    }
    override def isZero() = a.isZero()
  }
  
  //Unary operators
  abstract class UnaryOp(expr: Expression, name:String, f:Double => Double) extends Expression {
	  
      override def toString = name+"("+expr+")"
	
	  def value = expr.value match {
	    case Some(v) => Some(f(v))
	    case None => None
	  }
	
	  def eval(env : Var => Double) = f(expr.eval(env))
  }
  case class Log(expr : Expression) extends UnaryOp(expr:Expression, "log", math.log _) {
    def derive(v : Var) : Expression = {
		expr.derive(v) / expr
	}
    override def isZero() = expr match {
    		case Const(1) => true 
    		case _ => false
    }
  }
  case class Cos(expr: Expression) extends UnaryOp(expr:Expression, "cos", math.cos _) {
    def derive(v : Var) : Expression  = {
    	 Zero - expr.derive(v) * sin(expr)
    }
  }
  case class Sin(expr: Expression) extends UnaryOp(expr:Expression, "cos", math.sin _) {
    def derive(v : Var) : Expression = {
		expr.derive(v) * cos(expr)
	}
  }
  case class Tan(expr: Expression) extends UnaryOp(expr:Expression, "tan", math.tan _) {
    def derive(v : Var) : Expression = {
		expr.derive(v)/(cos(expr)*cos(expr))
	}
  }
  
  //Non linear functions
  def log(expr : Expression) = Log(expr)
  def sq(expr:Expression) = expr*expr
  //Trigo
  def cos(expr: Expression) = Cos(expr)
  def sin(expr: Expression) = Sin(expr)
  def tan(expr: Expression) = Tan(expr) 
  
//  def sum(exprs : Iterable[Expression]) : Expression = exprs.foldLeft(Zero : Expression)(_ + _)
//  
//  def sum[A](indexes : Iterable[A])(f : A => Expression) : Expression = sum(indexes map f)

  
  // -------------------- constraints --------------------
  
  object ConstraintType extends Enumeration {
    val LQ = Value("<=")
    val GQ = Value(">=")
    val EQ = Value("==")
  }
  
  

}
