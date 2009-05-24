package ca.nebvin.utils

import scala.util.parsing.combinator._

object Sass {
  def apply(in: String)= {
    val c = new SassCompiler
    c.parseAll(c.script, in)
  }
}

class SassCompiler extends JavaTokenParsers {
  override val whiteSpace = "".r

  def expr: Parser[Value] = cssValue~rep(op~cssValue) ^^ {
    case x~list => list.foldLeft(x)((r,n) => n match {case o~y => o(r,y)})
  }
  
  def cssValue: Parser[Value] = parens | length | number | color | string | failure("Not a valid value")

  def parens: Parser[Value] = sp~"("~sp~> expr <~sp~")"~sp
  
  def op: Parser[(Value, Value) => Value] =
    sp~> opt( "+" ^^^ {(l: Value, r: Value) => l + r}
            | "-" ^^^ {(l: Value, r: Value) => l - r}
            | "*" ^^^ {(l: Value, r: Value) => l * r}
            | "/" ^^^ {(l: Value, r: Value) => l / r}
            ) <~sp ^^ {_.getOrElse((l: Value, r: Value) => l << r)}

  def number: Parser[Number] = decimalNumber ^^ {x => Number(x.toDouble)}

  def string: Parser[Text] = (quotedString | unquotedString) ^^ {Text(_)}
  
  def quotedString: Parser[String] = "\"" ~> opt("""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})+""".r) <~ "\"" ^^ {_.getOrElse("")} 
  def unquotedString: Parser[String] = """[^()"]\S*""".r
  
  def length: Parser[Length] = number ~ unit ^^ {case Number(x)~u => Length(x, u)}
  def unit: Parser[String] = sp~>"em"|"px"|"pt"|"%"
  
  def color: Parser[Color] = longColor | shortColor
  
  def shortColor: Parser[Color] = "#"~>repN(3, hexColor(1)) ^^ {x =>
    val ints: List[Int] = x.map(h => hex2Int(h+h))
    Color(ints(0),ints(1),ints(2))
  }
  def longColor: Parser[Color] = "#"~>repN(3, hexColor(2)) ^^ {x =>
    val ints: List[Int] = x.map(h => hex2Int(h))
    Color(ints(0),ints(1),ints(2))
  }
  def hexColor(length: Int): Parser[String] = ("""[0-9a-fA-F]{"""+length+"}").r
  def hex2Int(hex: String): Int = Integer.valueOf(hex, 16).intValue()
  
  val ws = """\s*""".r
  val ws1 = """\s+""".r
  val sp = """\ *""".r
  val sp1 = """\ +""".r
  val lf = """\s*(\n|\z)""".r  
  
  def parseConstant(expression: String) = parseAll(expr, expression) match {
    case x if x.successful => x.get.toString
    case x => x.toString
  } 
  
  def replaceConstants(value: String, lookup: Map[String, String]): String = {
    val newValue = ConstantValueRegex.findAllIn(value).foldLeft(value)((r, m) => r.replace(m, lookup.getOrElse(m,m)))
    if (newValue == value) value else replaceConstants(newValue, lookup)
  }
  val ConstantValueRegex = """(!\S+)""".r
  val ShortColor = """#([0-9a-fA-F]{1})([0-9a-fA-F]{1})([0-9a-fA-F]{1})""".r
  val LongColor = """#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})""".r
  
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
    implicit def int2Number(in: Int) = Number(in.toDouble)
    implicit def value2Int(in: Value) = in match {
      case Number(x) => x.toInt
      case _ => 0
    }
  }

  case class Number(value: Double) extends Value {
    override def toString = {
      val whole = value.round
      if (whole.toDouble == value) whole.toString else value.toString
    }
    override def oper(that: Value, o:ValueOp, n:NumberOp): Value = that match {
      case Number(x) => Number(n(value,x))
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
      case Color(r,g,b) => Color(n(red,r).toInt, n(green,g).toInt, n(blue,b).toInt)
      case Number(n) => o(this,Color(n.toInt,n.toInt,n.toInt))
      case _ => o(this.asInstanceOf[Value],that)
    }
    private def colorHex(value: Int): String = value match {
      case x if x > 255 => "FF"
      case x if x < 0 => "00"
      case x => String.format("%02X", int2Integer(x))
    }
  }
  
  case class Length(number: Double, unit: String) extends Number(number) {
    override def toString = super.toString + unit
    override def + (that: Value): Value = that match {
      case Length(x,_) => Length(value + x, unit)
      case x => super.+(x)
    }
    override def - (that: Value): Value = that match {
      case Length(x,_) => Length(value - x, unit)
      case x => super.-(x)
    }
    override def * (that: Value): Value = that match {
      case Length(x,_) => Length(value * x, unit)
      case x => super.*(x)
    }
    override def / (that: Value): Value = that match {
      case Length(x,_) => Length(value / x, unit)
      case x => super./(x)
    }
  }

  def script: Parser[String] = ws~> rep1(rep(constant)~rep1(ruleset(0))) <~ws ^^ {
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
  
  def constant: Parser[Constant] = ("""!\S+""".r <~sp~"=")~value <~lf ^^ {
    case k~v => Constant(k,v)}
  
  def ruleset(curIndent: Int): Parser[RuleSet] =
    selectors(curIndent) ~ properties(curIndent + 2) ~ rep(ruleset(curIndent + 2)) ^? {
      case s~p~r if !(p.isEmpty && r.isEmpty) => RuleSet(s,p,r)}

  def selectors(curIndent: Int): Parser[List[Selector]] =
    indent(curIndent) ~> rep1sep(selector, ",") <~ lf
  
  def selector: Parser[Selector] =
    sp ~> """[^",:\n][^",\n]*""".r <~ sp ^^ {Selector(_)}
  
  def properties(curIndent: Int): Parser[List[Property]] =
    rep(property(curIndent)|nestedProperties(curIndent)) ^^ {pl => pl.flatMap(p => p)}
  
  def property(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> propertyName ~ value <~ lf ^^ {case p~v => List(Property(p,v))}
  
  def nestedProperties(curIndent: Int): Parser[List[Property]] =
    indent(curIndent) ~> (propertyName <~ lf) ~ rep1(property(curIndent + 2)) ^^ {
      case p~npl => npl.map(np => Property(p+"-"+np.head.name, np.head.value))}
  
  def propertyName: Parser[String] = ":" ~> """[a-zA-Z0-9\-]+""".r
  
  def value: Parser[String] = sp ~> """[^\n]+""".r <~ sp
  
  def indent(expected: Int): Parser[String] = ("""\ """+"{"+expected+"}").r 
  
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
}
