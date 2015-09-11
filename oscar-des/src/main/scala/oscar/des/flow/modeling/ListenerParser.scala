package oscar.des.flow.modeling

import javafx.beans.binding.{BooleanExpression, DoubleExpression}

import oscar.des.engine.Model
import oscar.des.flow.core.ItemClassHelper._
import oscar.des.flow.lib._
import oscar.examples.des.FactoryExample._
import scala.collection.immutable.SortedMap
import scala.util.parsing.combinator._

/**
 * Created by rdl on 08-09-15.
 */
class ListenerParser(storageFunction:String => Option[Storage],
                     processFunction:String => Option[ActivableProcess])
  extends RegexParsers with ListenersHelper{

  override def skipWhitespace: Boolean = true

  sealed class ListenerParsingResult
  case class DoubleExpressionResult(d:DoubleExpr) extends ListenerParsingResult
  case class BooleanExpressionResult(b:BoolExpr) extends ListenerParsingResult
  case class ParsingError(s:String) extends ListenerParsingResult {
    override def toString: String = "Parse Error:\n" + s + "\n"
  }

  def apply(input:String):ListenerParsingResult = {
    parseAll(expressionParser, input) match {
      case Success(result:BoolExpr, _) => BooleanExpressionResult(result)
      case Success(result:DoubleExpr, _) => DoubleExpressionResult(result)
      case n:NoSuccess => ParsingError(n.toString)
    }
  }

  def expressionParser:Parser[Expression] = doubleExprParser | boolExprParser

  def boolExprParser:Parser[BoolExpr] = (
    "empty(" ~> storageParser <~")" ^^ {empty(_)}
      | processBoolProbe("running",running)
      | processBoolProbe("anyBatchStarted",anyBatchStarted)
      | "true" ^^^ boolConst(true)
      | "false" ^^^ boolConst(false)
      | binaryOperatorBB2BParser("and",and)
      | binaryOperatorBB2BParser("or",or)
      | binaryOperatorBB2BParser("since",since)
      | unaryOperatorB2BParser("not",not)
      | unaryOperatorB2BParser("hasAlwaysBeen",hasAlwaysBeen)
      | unaryOperatorB2BParser("hasBeen",hasBeen)
      | unaryOperatorB2BParser("becomesTrue",becomesTrue)
      | unaryOperatorB2BParser("becomesFalse",becomesFalse)
      | binaryOperatorDD2BParser("g",g)
      | binaryOperatorDD2BParser("ge",ge)
      | binaryOperatorDD2BParser("l",l)
      | binaryOperatorDD2BParser("le",le)
      | binaryOperatorDD2BParser("eq",eq)
      | binaryOperatorDD2BParser("ne",neq)
      | "changed(" ~> (boolExprParser | doubleExprParser) <~")" ^^ {case e:Expression => changed(e)}
      | failure("expected boolean expression"))

  def doubleExprParser:Parser[DoubleExpr] = (
    storageDoubleProbe("stockLevel",stockLevel)
      | storageDoubleProbe("stockCapacity",stockCapacity)
      | storageDoubleProbe("relativeStockLevel",relativeStockLevel)
      | storageDoubleProbe("totalPut",totalPut)
      | storageDoubleProbe("totalFetch",totalFetch)
      | storageDoubleProbe("totalLosByOverflow",totalLosByOverflow)
      | processDoubleProbe("completedBatchCount",completedBatchCount)
      | processDoubleProbe("startedBatchCount",startedBatchCount)
      | processDoubleProbe("totalWaitDuration",totalWaitDuration)
      | doubleParser ^^ {d:Double => doubleConst(d)}
      | binaryOperatorDD2DParser("plus",plus)
      | binaryOperatorDD2DParser("minus",minus)
      | binaryOperatorDD2DParser("mult",mult)
      | binaryOperatorDD2DParser("div",(a,b) => div(a,b))
      | unaryOperatorD2DParser("opposite",opposite)
      | unaryOperatorD2DParser("delta",delta)
      | unaryOperatorB2DParser("cumulatedDuration",cumulatedDuration)
      | unaryOperatorB2DParser("cumulatedDurationNotStart",culumatedDurationNotStart)
      | "time"^^^ currentTime
      | "tic" ^^^ delta(currentTime)
      | unaryOperatorD2DParser("ponderateWithDuration",ponderateWithDuration)
      | "maxOnHistory(" ~> doubleExprParser~opt("," ~> boolExprParser)<~")" ^^ {
      case (d~None) => maxOnHistory(d)
      case (d~Some(cond:BoolExpr)) => maxOnHistory(d,cond)}
      | "minOnHistory(" ~> doubleExprParser~opt("," ~> boolExprParser)<~")"^^ {
      case (d~None) => minOnHistory(d)
      case (d~Some(cond:BoolExpr)) => minOnHistory(d,cond)}
      | unaryOperatorD2DParser("avgOnHistory",avgOnHistory)
      | failure("expected double expression"))


  //generic code

  //probes on storages
  def storageDoubleProbe(probeName:String,constructor:Storage=>DoubleExpr):Parser[DoubleExpr] =
    probeName~>"("~>storageParser <~")" ^^ {constructor(_)}
  def storageParser:Parser[Storage] = identifier convertStringUsingSymbolTable(storageFunction, "storage")

  //probes on processes
  def processDoubleProbe(probeName:String,constructor:ActivableProcess=>DoubleExpr):Parser[DoubleExpr] =
    probeName~>"("~>processParser <~")" ^^ {constructor(_)}
  def processBoolProbe(probeName:String,constructor:ActivableProcess=>BoolExpr):Parser[BoolExpr] =
    probeName~>"("~>processParser <~")" ^^ {constructor(_)}
  def processParser:Parser[ActivableProcess] = identifier convertStringUsingSymbolTable(processFunction, "process")

  // some generic parsing methods
  def unaryOperatorD2DParser(operatorString:String,constructor:DoubleExpr=>DoubleExpr):Parser[DoubleExpr] =
    operatorString~>"("~>doubleExprParser<~")" ^^ {
      case param => constructor(param)
    }

  def unaryOperatorB2BParser(operatorString:String,constructor:BoolExpr=>BoolExpr):Parser[BoolExpr] =
    operatorString~>"("~>boolExprParser<~")" ^^ {
      case param => constructor(param)
    }

  def unaryOperatorB2DParser(operatorString:String,constructor:BoolExpr=>DoubleExpr):Parser[DoubleExpr] =
    operatorString~>"("~>boolExprParser<~")" ^^ {
      case param => constructor(param)
    }

  def binaryOperatorDD2DParser(operatorString:String,constructor:(DoubleExpr,DoubleExpr)=>DoubleExpr):Parser[DoubleExpr] =
    operatorString~"("~>doubleExprParser~(","~>doubleExprParser<~")") ^^ {
      case param1~param2 => constructor(param1,param2)
    }

  def binaryOperatorDD2BParser(operatorString:String,constructor:(DoubleExpr,DoubleExpr)=>BoolExpr):Parser[BoolExpr] =
    operatorString~>"("~>doubleExprParser~(","~>doubleExprParser<~")") ^^ {
      case param1~param2 => constructor(param1,param2)
    }

  def binaryOperatorBB2BParser(operatorString:String,constructor:(BoolExpr,BoolExpr)=>BoolExpr):Parser[BoolExpr] =
    operatorString~>"("~>boolExprParser~(","~>boolExprParser<~")") ^^ {
      case param1~param2 => constructor(param1,param2)
    }

  class parserWithSymbolTable(identifierParser:Parser[String]){
    def convertStringUsingSymbolTable[U](symbolTable:String=>Option[U],symbolType:String):Parser[U] = new Parser[U] {
      def apply(in: Input) = identifierParser(in) match {
        case Success(x, in1) => symbolTable(x) match{
          case Some(u:U) => Success(u, in1)
          case None => Failure("" + x + " is not a known " + symbolType,in)
        }
        case f:Failure => f
        case e:Error => e
      }
    }
  }
  implicit def addSymbolTableFeature(identifierParser:Parser[String]):parserWithSymbolTable = new parserWithSymbolTable(identifierParser)

  def identifier:Parser[String] = """[a-zA-Z0-9]+""".r ^^ {_.toString}

  def doubleParser:Parser[Double] = """[0-9]+(\.[0-9]+)?""".r ^^ {case s:String => println("converting" + s);s.toDouble}
}

object ParserTester extends App with FactoryHelper{

  val m = new Model
  val aStorage = new FIFOStorage(10,Nil,"aStorage",false,false)
  val bStorage = new FIFOStorage(10,Nil,"bStorage",false,false)
  val storages = SortedMap("aStorage"->aStorage,"bStorage" -> bStorage)

  val aProcess = new SingleBatchProcess(m, 5000, Array(), Array((()=>1,aStorage)), identity, "aProcess", verbose)
  val bProcess = new SingleBatchProcess(m, 5000, Array(), Array((()=>1,aStorage)), identity, "bProcess", verbose)
  val processes = SortedMap("aProcess"->aProcess,"bProcess" -> bProcess)

  val myParser = new ListenerParser(storageFunction = (s:String) => storages.get(s),
    processFunction = (s:String) => processes.get(s))


  println(myParser("mult(completedBatchCount(aProcess),totalPut(aStorage))"))

  println(myParser("cumulatedDuration(empty(bStorage))"))
  println(myParser("cumulatedDuration(not(hasBeen(running(cProcess))))"))
  println(myParser("empty(aStorage)"))
  println(myParser("cumulatedDuration(not(running(bProcess)))"))
  println(myParser("cumulatedDurationNotStart(not(running(aProcess)))"))
  println(myParser("maxOnHistory(stockLevel(aStorage))"))
  println(myParser("minOnHistory(stockLevel(aStorage))"))
  println(myParser("avgOnHistory(relativeStockLevel(bStorage))"))
  println(myParser("avgOnHistory(stockLevel(aStorage))"))
  println(myParser("ponderateWithDuration(stockLevel(bStorage))"))
}