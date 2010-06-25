package ca.fyrie.utils

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.immutable.PagedSeq
import CharArrayReader.EofCh

object Sass {
  def apply(in: String)= {
    val c = new SassCompiler
    c.parse(in)
  }
}

class SassCompiler extends Parsers {
  type Elem = Char

  def expr: Parser[Value] = value~rep(op~value) ^^ {
    case x~list => list.foldLeft(x)((r,n) => n match {case o~y => o(r,y)})
  }
  
  def value: Parser[Value] = (parens | length | number | color | text | failure("Not a valid value"))

  def parens: Parser[Value] = sp~'('~sp~> expr <~sp~')'~sp
  
  def op: Parser[(Value, Value) => Value] =
    sp~> opt( '+' ^^^ {(l: Value, r: Value) => l + r}
            | '-' ^^^ {(l: Value, r: Value) => l - r}
            | '*' ^^^ {(l: Value, r: Value) => l * r}
            | '/' ^^^ {(l: Value, r: Value) => l / r}
            ) <~sp ^^ {_.getOrElse((l: Value, r: Value) => l << r)}

  def number: Parser[Number] = num ^^ {x => Number(x.toDouble)}

  def length: Parser[Length] = number ~ unit ^^ {case x~u => Length(x, u)}
  def unit: Parser[String] = sp~>"em"|"px"|"pt"|"%"

  def text: Parser[Text] = (string | ident) ^^ {Text(_)}

  def color: Parser[Color] = longColor | shortColor
  
  def shortColor: Parser[Color] =
    '#' ~> repN(3, hexDigit) ^^ {case List(r,g,b) => Color(hex2Int(r+r),hex2Int(g+g),hex2Int(b+b))}

  def longColor: Parser[Color] =
    '#' ~> repN(6, hexDigit) ^^ {case List(r1,r2,g1,g2,b1,b2) => Color(hex2Int(r1+r2),hex2Int(g1+g2),hex2Int(b1+b2))}

  def hexDigit: Parser[String] = (digit|"a"|"b"|"c"|"d"|"e"|"f"|"A"|"B"|"C"|"D"|"E"|"F") ^^ (_.toString)

  def hex2Int(hex: String): Int = Integer.valueOf(hex, 16).intValue()

  def sp: Parser[String] = rep(' ') ^^ {_.mkString}
  def sp1: Parser[String] = rep1(' ') ^^ {_.mkString}
  def lf: Parser[String] = rep1(sp ~ "\n") ^^ {_.mkString}
  def ws: Parser[String] = rep("\n"|"\t"|" ") ^^ {_.mkString}
  def eof: Parser[Char] = EofCh
  
  def parseConstant(expression: String) = parse(expr, expression) match {
    case x if x.successful => x.get.toString
    case x => x.toString
  } 
  
  def replaceConstants(value: String, lookup: Map[String, String]): String = {
    val newValue = ConstantValueRegex.findAllIn(value).foldLeft(value)((r, m) => r.replace(m, lookup.getOrElse(m,m)))
    if (newValue == value) value else replaceConstants(newValue, lookup)
  }
  val ConstantValueRegex = """(!\S+)""".r
  
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

  def script: Parser[String] = rep1(rep(constant)~rep1(ruleset(0))) <~ eof ^^ {
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
  
  def constant: Parser[Constant] = ('!' ~> rep1(ident|num|'-') <~sp~'=')~propertyValue <~lf ^^ {
    case k~v => Constant("!"+k.mkString,v)}
  
  def ruleset(curIndent: Int): Parser[RuleSet] =
    selectors(curIndent) ~ properties(curIndent + 2) ~ rep(ruleset(curIndent + 2)) ^? {
      case s~p~r if !(p.isEmpty && r.isEmpty) => RuleSet(s,p,r)}

  def selectors(curIndent: Int): Parser[List[Selector]] =
    indent(curIndent) ~> rep1sep(selector, ',') <~ lf
  
  def selector: Parser[Selector] =
    sp ~> rep1(ident|num|'#'|'.'|'&'|':'|'-'|sp1) <~ sp ^^ {x => Selector(x.mkString)}
  
  def properties(curIndent: Int): Parser[List[Property]] =
    rep(property(curIndent)|nestedProperties(curIndent)) ^^ {pl => pl.flatMap(p => p)}
  
  def property(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> propertyName ~ propertyValue <~ lf ^^ {case p~v => List(Property(p,v))}
  
  def nestedProperties(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> (propertyName <~ lf) ~ rep1(property(curIndent + 2)) ^^ {
      case p~npl => npl.map(np => Property(p+"-"+np.head.name, np.head.value))}
  
  def propertyName: Parser[String] = ':' ~> rep1(ident|'-') ^^ {_.mkString}
  
  def propertyValue: Parser[String] = sp1 ~> rep(ident|num|string|sp1|'='|'#'|'/'|'!'|'-'|'+'|'('|')') ^^ {_.mkString}
  
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

  def letter = elem("letter", _.isLetter)

  def digit = elem("digit", _.isDigit)

  def ident: Parser[String] = rep1(letter|'_') ^^ (_.mkString)

  def num: Parser[String] = rep1(digit) ^^ (_.mkString)

  def string = '"' ~> rep(elem("any", (x => true)))  <~ '"' ^^ (_.mkString)

  implicit def literal(in: Char): Parser[Char] = elem(in)

  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      var i = 0
      var j = offset
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(offset, j).toString, in.drop(j - offset))
      else 
        Failure("`"+s+"' expected but `"+in.first+"' found", in)
    }
  }

  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = p(in)
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = p(new CharSequenceReader(in))
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = p(new PagedSeqReader(PagedSeq.fromReader(in)))
  def parse(in: Reader[Char]): ParseResult[String] = parse(script,in)
  def parse(in: java.lang.CharSequence): ParseResult[String] = parse(script,in)
  def parse(in: java.io.Reader): ParseResult[String] = parse(script,in)
}
