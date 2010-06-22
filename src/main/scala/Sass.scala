package ca.fyrie.utils

import scala.util.parsing.combinator._
import lexical.StdLexical
import syntactical.StdTokenParsers
import token.StdTokens

object Sass {
  def apply(in: String)= {
    val c = new SassCompiler
    c.parse(in)
  }
}

class SassCompiler extends StdTokenParsers with PackratParsers with ImplicitConversions {
  type Tokens = SassTokens
  import lexical.NumericColor
  
  override val lexical = new SassLexer

  val expr: PackratParser[Value] = value~rep(op~value) ^^ {
    case x~list => list.foldLeft(x)((r,n) => n match {case o~y => o(r,y)})
  }
  
  val value: PackratParser[Value] = parens | length | number | color | string | failure("Not a valid value")

  def parens: Parser[Value] = sp~"("~sp~> expr <~sp~")"~sp
  
  def op: Parser[(Value, Value) => Value] =
    sp~> opt( "+" ^^^ {(l: Value, r: Value) => l + r}
            | "-" ^^^ {(l: Value, r: Value) => l - r}
            | "*" ^^^ {(l: Value, r: Value) => l * r}
            | "/" ^^^ {(l: Value, r: Value) => l / r}
            ) <~sp ^^ {_.getOrElse((l: Value, r: Value) => l << r)}

  def number: Parser[Number] = numericLit ^^ {x => Number(x.toDouble)}

  def string: Parser[Text] = (stringLit | ident) ^^ {Text(_)}

  def length: Parser[Length] = number ~ unit ^^ {case x~u => Length(x, u)}
  def unit: Parser[String] = sp~>"em"|"px"|"pt"|"%"
  
  // This is ugly... refactor later
  def color: Parser[Color] = numericColor ^^ { x =>
    (x.length match {
      case 3 => x.grouped(1).map(d => hex2Int(d+d)).toList
      case 6 => x.grouped(2).map(hex2Int).toList
    }) match {
      case r :: g :: b :: Nil => Color(r,g,b)
    }
  }

  def hex2Int(hex: String): Int = Integer.valueOf(hex, 16).intValue()

  def sp: Parser[String] = rep(" ") ^^ {_.mkString}
  def sp1: Parser[String] = rep1(" ") ^^ {_.mkString}
  val lf: Parser[String] = rep1(sp ~ "\n") ^^ {_.mkString}
  
  def parseConstant(in: String) = {
    phrase(expr)(new lexical.Scanner(in)) match {
      case Success(res, _) => res.toString
      case x => x.toString
    }
  }
  
  def replaceConstants(value: String, lookup: Map[String, String]): String = {
    val newValue = ConstantValueRegex.findAllIn(value).foldLeft(value)((r, m) => r.replace(m, lookup.getOrElse(m,m)))
    if (newValue == value) value else replaceConstants(newValue, lookup)
  }
  val ConstantValueRegex = """(!\S+)""".r
//  val ShortColor = """#([0-9a-fA-F]{1})([0-9a-fA-F]{1})([0-9a-fA-F]{1})""".r
//  val LongColor = """#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})""".r
  
  sealed abstract class Value {
    type ValueOp = (Value,Value) => Value
    type NumberOp = (Double,Double) => Double
    def << (that: Value): Value = Text(toString+" "+that)
    def + (that: String): Text = Text(toString+that)
    def + (that: Value): Value = oper(that, (_+_), (_+_))
    def - (that: Value): Value = oper(that, (_-_), (_-_))
    def * (that: Value): Value = oper(that, (_*_), (_*_))
    def / (that: Value): Value = oper(that, (_/_), (_/_))
    protected def oper(that: Value, o:ValueOp, n:NumberOp): Value = o(Text(toString), that)
  }

  object Value {
    implicit def text2String(in: Text) = in.toString
    implicit def double2Number(in: Double) = Number(in)
    implicit def number2Double(in: Number) = in.toDouble
  }

  case class Number(value: Double) extends Value {
    override def toString = """\.0+$""".r.replaceFirstIn(value.toString, "")
    def toDouble = value
    override def oper(that: Value, o:ValueOp, n:NumberOp): Value = that match {
      case Number(x) => Number(n(value,x))
      case Length(x, u) => Length(n(value,x), u)
      case _ => super.oper(that,o,n)
    }
  }

  case class Text(value: String) extends Value {
    override def + (that: Value): Value = this+that.toString
    override def - (that: Value): Value = this+"-"+that.toString
    override def * (that: Value): Value = this+"*"+that.toString
    override def / (that: Value): Value = this+"/"+that.toString
    override def toString = value
  }
  
  case class Color(red: Int, green: Int, blue: Int) extends Value {
    override def toString = "#" + colorHex(red) + colorHex(green) + colorHex(blue)
    override def oper(that: Value, o: ValueOp, n: NumberOp): Value = that match {
      case Color(r,g,b) => Color(n(red,r), n(green,g), n(blue,b))
      case Number(n) => o(this,Color(n,n,n))
      case _ => super.oper(that,o,n)
    }
    private def colorHex(value: Int): String = value match {
      case x if x > 255 => "ff"
      case x if x < 0 => "00"
      case x => String.format("%02x", int2Integer(x))
    }
    private implicit def double2Int(in: Double): Int = in.toInt
  }
  
  case class Length(value: Number, unit: String) extends Value {
    override def toString = value + unit
    override def oper(that: Value, o:ValueOp, n:NumberOp): Value = that match {
      case Length(x,_) => Length(o(value,x).asInstanceOf[Number], unit)
      case x: Number => Length(o(value,x).asInstanceOf[Number], unit)
      case _ => super.oper(that,o,n)
    }
  }

  def script: Parser[String] = rep1(rep(constant)~rep1(ruleset(0))) ^^ {
    _.foldLeft(("",Map[String,String]())){(r,s) =>
      val (css, constantMap) = r
      s match {
        case constants~rulesets => {
          val newMap = constants.foldLeft(constantMap){(m,c) =>
            m ++ Map(c.name -> parseConstant(replaceConstants(c.value,m)))}
          (css + rulesets.map(_.toString(newMap, Nil)).mkString, newMap)
        }
      }
    }._1
  }
  
  def constant: Parser[Constant] = ("!"~> ident <~sp~"=")~propertyValue <~lf ^^ {
    case k~v => Constant(k,v)}
  
  def ruleset(curIndent: Int): Parser[RuleSet] =
    selectors(curIndent) ~ properties(curIndent + 2) ~ rep(ruleset(curIndent + 2)) ^? {
      case s~p~r if !(p.isEmpty && r.isEmpty) => RuleSet(s,p,r)}

  def selectors(curIndent: Int): Parser[List[Selector]] =
    indent(curIndent) ~> rep1sep(selector, ",") <~ lf
  
  def selector: Parser[Selector] =
    sp ~> ident <~ sp ^^ {Selector(_)}
  
  def properties(curIndent: Int): Parser[List[Property]] =
    rep(property(curIndent)|nestedProperties(curIndent)) ^^ {pl => pl.flatMap(p => p)}
  
  def property(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> propertyName ~ propertyValue <~ lf ^^ {case p~v => List(Property(p,v))}
  
  def nestedProperties(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> (propertyName <~ lf) ~ rep1(property(curIndent + 2)) ^^ {
      case p~npl => npl.map(np => Property(p+"-"+np.head.name, np.head.value))}
  
  def propertyName: Parser[String] = ":" ~> ident
  
  def propertyValue: Parser[String] = sp ~> ident <~ sp
  
  def indent(expected: Int): Parser[String] = repN(expected, " ") ^^ (_.mkString)
  
  case class Property(val name: String, val value: String) {
    override def toString = name + ":" + value + ";"
    def toString(constants: Map[String, String]): String = value match {
      case ConstantValueRegex(x) =>
        Property(name, parseConstant(replaceConstants(x,constants))).toString
      case x => toString
    }
    private val ConstantValueRegex = """=\s+(.*)""".r
  }

  case class Selector(val name: String) {
    override def toString = name
  }

  case class RuleSet(val selectors: List[Selector], val properties: List[Property], val children: List[RuleSet]) {
    override def toString: String = toString(Map(), Nil)
    def toString(constants: Map[String, String], parentSelectors: List[Selector]): String = {
      val curSelectors = parentSelectors match {
        case Nil => selectors
        case _ => parentSelectors.flatMap(p => selectors.map(s =>
          if (s.name.contains("&")) Selector(s.name.replace("&", p.name)) else Selector(p.name+" "+s.name)))
      }
      (if (properties.isEmpty) "" else (curSelectors.mkString(", ")+" { "+properties.map(_.toString(constants)).mkString(" ")+" }\n"))+children.map(_.toString(constants, curSelectors)).mkString
    }
  }

  case class Constant(val name: String, val value: String)

  def numericColor: Parser[String] =
    elem("numeric color", _.isInstanceOf[NumericColor]) ^^ (_.chars)

  def parse(in: String) = {
    phrase(script)(new lexical.Scanner(in)) /*match {
      case Success(res, _) => res
      case x => x.toString
    }*/
  }

}

class SassLexer extends StdLexical with SassTokens {
  override def token: Parser[Token] = numericColorToken | floatingToken | super.token

  def numericColorToken: Parser[Token] =
    '#' ~> (repN(6, hexDigit)|repN(3, hexDigit)) ^^ {case chars => NumericColor(chars mkString "")}

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  def floatingToken: Parser[Token] =
    rep1(digit) ~ optFraction ~ optExponent ^^ {
      case intPart ~ frac ~ exp => NumericLit(
        (intPart mkString "") :: frac :: exp :: Nil mkString "")}

  def chr(c:Char) = elem("", ch => ch==c )
  def sign = chr('+') | chr('-')
  def optSign = opt(sign) ^^ {
    case None => ""
    case Some(sign) => sign
  }

  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }

  def optFraction = opt(fraction) ^^ {
    case None => ""
    case Some(fraction) => fraction
  }

  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }

  def optExponent = opt(exponent) ^^ {
    case None => ""
    case Some(exponent) => exponent
  }

}

trait SassTokens extends StdTokens {
  
  case class NumericColor(chars: String) extends Token {
    override def toString = chars
  }

}
