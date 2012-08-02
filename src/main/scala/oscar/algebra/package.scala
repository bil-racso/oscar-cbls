package oscar

import oscar.linprog.modeling._

package object algebra {
  
  // some useful linear algebra functions
  
  def min(a:Double,b:Double): Double = {a.min(b)}
  def max(a:Double,b:Double): Double = {a.max(b)}
  
  
  def sumNum[A](indexes1:Iterable[A])(f: A => Double) : Double = {
    (for(i<-indexes1) yield f(i)).sum
  }
  
  def sumNum[A,B](indexes1 : Iterable[A],indexes2 : Iterable[B])(f: (A,B) => Double) : Double = {
    (for(i<-indexes1;j<-indexes2) yield f(i,j)).sum
  }
  
  def sumNum[A,B,C](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3: Iterable[C])(f: (A,B,C) => Double) : Double = {
    (for(i<-indexes1;j<-indexes2;k<-indexes3) yield f(i,j,k)).sum
  }
  
  def sumNum[A,B,C,D](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C], indexes4 : Iterable[D])(f : (A,B,C,D) => Double) : Double = {
    (for(i <- indexes1;j <- indexes2; k<- indexes3; l <- indexes4) yield f(i,j,k,l)).sum
  } 
  
  def createVarMap[T,V](ts:Seq[T])(varConstr:T=>V): Map[T,V] = { (for (t<-ts) yield t-> varConstr(t)).toMap }  
  
  
  // -------------------------  linear expressions & constraints -------------------

  /**Abstract type for variables*/

  
  abstract class LinearExpression extends Expression {
    
    val cte: Double
    val coef: scala.collection.immutable.Map[Var,Double]
    
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
    
    override def derive(x: Var): Expression = {
     coef.get(x) match {
       case None => Zero
       case Some(v: Double) => new Const(v)
     }
    }
  }
  
  class Const (val d : Double) extends LinearExpression {
    
    val cte = d
    val coef = scala.collection.immutable.Map[Var,Double]()
    
    def *(expr : LinearExpression) : LinearExpression = new LinearExpressionProd(this,expr)

    def *(c2 : Const) = new Const(d*c2.d)
    
    def + (c2: Const) = new Const(d+c2.d)
    
    def - (c2: Const) = new Const(d-c2.d)
    
    def *(x : Var) = new CstVar(this,x)
    
    override def toString = d.toString
    
    override def derive(v : Var) : Expression = Zero  
    
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
    val coef = scala.collection.immutable.Map(this->1.0)
    
    override def toString = name

    
    override def derive(v : Var) : Expression = {
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
  abstract class LinearExpressionBinary(expr1: LinearExpression,expr2: LinearExpression) extends LinearExpression {
   
    val cte = op(expr1.cte,expr2.cte)
    val coef = merge()    
      
    def op(v1: Double, v2: Double): Double
    
    val opStr: String
       

    override def toString() = "("+expr1+ opStr +expr2+")"
    
    
    def merge() : scala.collection.immutable.Map[Var,Double] = {
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
       import scala.collection.immutable.Map
       mymap.filterNot(_._2 == 0).toMap
    }
   
  
  }  
  
  /**
   * (linExpr1 + linExpr2)
   */
  class LinearExpressionSum(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1,expr2) {
        
    val opStr = "+"

    def op(v1: Double, v2: Double) = v1 + v2
    
    override def derive(v : Var) : Expression = {
		expr1.derive(v) + expr2.derive(v)
	}
  }
  
  /**
   * (linExpr1 - linExpr2)
   */
  class LinearExpressionDiff(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1,expr2) {
    val opStr = "-"

    def op(v1: Double, v2: Double) = v1 - v2
    
    override def derive(v : Var) : Expression = {
		expr1.derive(v) - expr2.derive(v)
	}
  }  
  
  /**
   * (c * linExpr)
   */
  class LinearExpressionProd(val c: Const, val expr: LinearExpression) extends LinearExpression {
    import scala.collection.immutable.Map
    val cte = if (c == Zero) 0.0 else c.d * expr.cte
    val coef = if (c == Zero) Map[Var,Double]() else expr.coef.map(e => (e._1 -> c.d *e._2))   
      

    override def derive(v : Var) : Expression = {
		c * expr.derive(v)
	}
    override def toString = c+"*(" + expr + ")"
  
  }  


  /**
   * (c * x)
   */
  class CstVar(val coeff: Const, val variable: Var) extends LinearExpression {

    import scala.collection.immutable.Map
    val cte = 0.0
    val coef = if (coeff == 0) Map[Var,Double]() else Map(variable -> coeff.d)

     
    override def toString = coeff + "*" + variable
    
    override def derive(v : Var) : Expression = {
		if (variable == v) coeff
		else Zero
	}
   
  }
  
  object CstVar {
    def unapply(l : CstVar) : Option[(Const, Var)] = Some(l.coeff, l.variable)
  }
  
  
  
  def sum(exprs : Iterable[LinearExpression]) : LinearExpression = {
    import scala.collection.mutable.Map
    val mymap = Map[Var,Double]()
    var mycte = 0.0
    for (expr <- exprs) {
      mycte += expr.cte
      for((k,v) <- expr.coef) {
        mymap.get(k) match {
           case Some(c) => mymap(k) = c+v
           case None => mymap += (k -> v)
        }
      }
    }
    import scala.collection.immutable.Map
    mymap.filterNot(_._2 == 0)
    new LinearExpression() {
      val cte = mycte
      val coef = mymap.toMap
    }
    
    

    //exprs.foldLeft(Zero : LinearExpression)(_ + _)
  }
  
  /**
   * sum[a <- A] f(a)
   */ 
  def sum[A](indexes : Iterable[A])(f : A => LinearExpression) : LinearExpression = sum(indexes map f)
  
  /**
   * sum[a <- A, b <- B] f(a,b)
   */ 
  def sum[A,B](indexes1 : Iterable[A],indexes2 : Iterable[B])(f : (A,B) => LinearExpression) : LinearExpression = {
         sum(for(i <- indexes1;j <- indexes2) yield f(i,j))
  }
  
  /**
   * sum[a <- A, b <- B, c <- C] f(a,b,c)
   */ 
  def sum[A,B,C](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C])(f : (A,B,C) => LinearExpression) : LinearExpression = {
    	sum(for(i <- indexes1;j <- indexes2; k <- indexes3) yield f(i,j,k))
  }

  /**
   * sum[a <- A, b <- B, c <- C, d <- D] f(a,b,c,d)
   */ 
  def sum[A,B,C,D](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C], indexes4 : Iterable[D])(f : (A,B,C,D) => LinearExpression) : LinearExpression = {
    	sum(for(i <- indexes1;j <- indexes2; k<- indexes3; l <- indexes4) yield f(i,j,k,l))
  }
  
  /**
   * sum[a <- A such that filter(a) == true] f(a)
   */
  def sum[A](S1:Iterable[A], filter: A => Boolean, f:(A) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; if(filter(a)))  yield f(a))  
  }
  
  /**
   * sum[a <- A, b <- B such that filter(a,b) == true] f(a,b)
   */
  def sum[A,B](S1:Iterable[A],S2:Iterable[B], filter: (A,B) => Boolean, f:(A,B) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; if(filter(a,b)))  yield f(a,b))  
  }

  /**
   * sum[a <- A, b <- B, c <- C such that filter(a,b,c) == true] f(a,b,c)
   */  
  def sum[A,B,C](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C], filter: (A,B,C) => Boolean, f:(A,B,C) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; c <- S3; if(filter(a,b,c)))  yield f(a,b,c))  
  }
  
  /**
   * sum[a <- A, b <- B, c <- C, d <- D such that filter(a,b,c,d) == true] f(a,b,c,d)
   */    
  def sum[A,B,C,D](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C],S4:Iterable[D], filter: (A,B,C,D) => Boolean, f:(A,B,C,D) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; c <- S3; d <- S4; if(filter(a,b,c,d)))  yield f(a,b,c,d))  
  }  
  

  // ------------   Implicits -----------------------
  
  implicit def double2const(d : Double) : Const = if (d == 0.0) Zero else Const(d)
  implicit def int2const(i : Int) : Const = double2const(i)
  
  // --------------- Linear Constraints -------------
  
  /**
   * A linear constraint has the form (linearExpression REL 0) with REL in {<=, ==, >=}
   */
  class LinearConstraint(val linExpr: LinearExpression, val consType: ConstraintType.Value) {
    override def toString = linExpr+" "+consType+" "+0
  }
  
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
