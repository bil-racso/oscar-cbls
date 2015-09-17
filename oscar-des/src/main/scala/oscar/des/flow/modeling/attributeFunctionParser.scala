package oscar.des.flow.modeling

import oscar.des.flow.core._
import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator._

sealed abstract class ParsingResultItemClassTransformFunction()
case class ParsedTransformFuntion(f:ItemClassTransformFunction) extends ParsingResultItemClassTransformFunction
case class ParseErrorTransformFunction(s:String) extends ParsingResultItemClassTransformFunction

sealed abstract class ParsingResultItemClassTransformWitAdditionalOutput()
case class ParsedItemClassTransformWitAdditionalOutput(f:ItemClassTransformWitAdditionalOutput) extends ParsingResultItemClassTransformWitAdditionalOutput
case class ParseErrorItemClassTransformWitAdditionalOutput(s:String) extends ParsingResultItemClassTransformWitAdditionalOutput

class AttributeFunctionParser(attributes:AttributeDefinitions)
  extends RegexParsers with AttributeHelper {
  override def skipWhitespace: Boolean = true

  def parseTransformFunction(input: String): ParsingResultItemClassTransformFunction = {
    parseAll(attributeTransformFunctionParser, input) match {
      case Success(result: ItemClassTransformFunction, _) => ParsedTransformFuntion(result)
      case n: NoSuccess => ParseErrorTransformFunction(n.toString)
    }
  }

  def parseTransformFunctionWithAdditionalOutput(input:String):ParsingResultItemClassTransformWitAdditionalOutput = {
    parseAll(transformFunctionWithOutputParser, input) match {
      case Success(result: ItemClassTransformWitAdditionalOutput, _) => ParsedItemClassTransformWitAdditionalOutput(result)
      case n: NoSuccess => ParseErrorItemClassTransformWitAdditionalOutput(n.toString)
    }
  }

  private def transformFunctionWithOutputParser:Parser[ItemClassTransformWitAdditionalOutput] = (
    "if"~>attribute~("then"~>(transformFunctionWithOutputParser~("else"~>transformFunctionWithOutputParser))) ^^ {
      case ((attr:Attribute)~((thenF:ItemClassTransformWitAdditionalOutput)~(elseF:ItemClassTransformWitAdditionalOutput))) => iTE(attr,thenF,elseF)}
      |"outputPort"~>integer~atomicAttributeTransformFunctionParser^^{case i~f => outputValue(()=>i,f)}
    )

  private def attributeTransformFunctionParser: Parser[ItemClassTransformFunction] =
    rep(atomicAttributeTransformFunctionParser) ^^ {
      case Nil => identity
      case h :: Nil => h
      case l: List[ItemClassTransformFunction] => composedItemClassTransformFunction(l: _*)
    }

  private def atomicAttributeTransformFunctionParser: Parser[ItemClassTransformFunction] = (
    "add" ~> attribute ^^ { a: Attribute => addAttribute(a) }
      | "remove" ~> attribute ^^ { a: Attribute => removeAttribute(a) }
      | "const{" ~> (rep(attribute) <~ "}") ^^ { c: List[Attribute] => constantAttributes(attributeSet(SortedSet.empty[Attribute] ++ c, attributes)) }
    )

  private def attribute: Parser[Attribute] =
    identifier convertStringUsingSymbolTable(attributes.optionGet, "attribute")

  private def identifier: Parser[String] = """[a-zA-Z0-9]+""".r ^^ {_.toString}

  private def integer:Parser[Int] = """[0-9]+""".r ^^ {_.toInt}

  private implicit def addSymbolTableFeature(identifierParser: Parser[String]): parserWithSymbolTable = new parserWithSymbolTable(identifierParser)

  private class parserWithSymbolTable(identifierParser: Parser[String]) {
    def convertStringUsingSymbolTable[U](symbolTable: String => Option[U], symbolType: String): Parser[U] = new Parser[U] {
      def apply(in: Input) = identifierParser(in) match {
        case Success(x, in1) => symbolTable(x) match {
          case Some(u: U) => Success(u, in1)
          case None => Failure("" + x + " is not a known " + symbolType, in)
        }
        case f: Failure => f
        case e: Error => e
      }
    }
  }
}
