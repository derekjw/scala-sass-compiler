package ca.nebvin.utils

import scala.util.parsing.combinator._

object ConstantCalculator  extends JavaTokenParsers {
    
  def expr: Parser[CSSValue] = value ~ opt(op~expr) ^^ {
    case x~None => x
    case x~Some(o~y) => o match {
      case Op("+") => x+y
      case Op("<<") => x<<y}}
  
  def value: Parser[CSSValue] = ("(" ~> expr <~ ")") | length | color | string 
  
  def op: Parser[Op] = opt("+" | "-" | "*" | "/") ^^ {
    case None => Op("<<")
    case Some(x) => Op(x)}

  def string: Parser[CSSString] = (quotedString | unquotedString) ^^ {CSSString(_)}
  
  def quotedString: Parser[String] = "\"" ~> opt("""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})+""".r) <~ "\"" ^^ {_.getOrElse("")} 
  def unquotedString: Parser[String] = """[^()]\S*""".r
  
  def length: Parser[CSSLength] = (decimalNumber ~ opt(unit) ^^ {case x~u => CSSLength(x.toDouble, u)}) ~ opt(op~length) ^^ {
    case x~None => x
    case x~Some(o~y) => o match {
      case Op("-") => x - y
      case Op("+") => x + y
      case Op("*") => x * y
      case Op("/") => x / y}}
  def unit: Parser[String] = "em" | "px" | "pt" | "%"
  
  def color: Parser[CSSColor] = ((longColor | shortColor | wholeNumber) ^^ {CSSColor(_)} ^? {case Some(x) => x}) ~ opt(op~color) ^^ {
    case x~None => x
    case x~Some(Op(o)~y) => o match {
      case "-" => x-y
      case _ => x+y
    }}
  
  def shortColor: Parser[String] = hexColor(3)
  def longColor: Parser[String] = hexColor(6)
  def hexColor(length: Int): Parser[String] = ("""#[0-9a-fA-F]"""+"{"+length+"}").r
  
  def parse(expression: String) = parseAll(expr, expression) match {
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
  
  trait CSSValue {
    def + (that: CSSValue): CSSValue
    def + (that: String): CSSString = CSSString(toString+that)
    def << (that: CSSValue): CSSString = CSSString(toString+" "+that)
  }
  
  object CSSString {
    def apply(value: String) = new CSSString(value)
  }
  
  class CSSString(val value: String) extends CSSValue {
    override def + (that: CSSValue) = CSSString(value + that.toString)
    override def toString = value
  }
  
  object CSSColor {
    def apply(value:String) = value match {
      case LongColor(red, green, blue)  => Some(new CSSColor(red,green,blue))
      case ShortColor(red, green, blue) => Some(new CSSColor(red+red,green+green,blue+blue))
      case rgb if (rgb == rgb.toInt.toString) => Some(new CSSColor(rgb.toInt, rgb.toInt, rgb.toInt))
      case _ => None}
    def apply(red: Int, green: Int, blue: Int) = new CSSColor(red, green, blue)
  }

  class CSSColor(val red: Int, val green: Int, val blue: Int) extends CSSValue {
    def this(strRed: String, strGreen: String, strBlue: String) =
      this(Integer.valueOf(strRed, 16).intValue(),
           Integer.valueOf(strGreen, 16).intValue(),
           Integer.valueOf(strBlue, 16).intValue())
    override def toString = "#" + colorHex(red) + colorHex(green) + colorHex(blue)
    override def + (that: CSSValue) = CSSString(toString + that)
    def + (that: CSSColor): CSSColor = CSSColor(red + that.red, green + that.green, blue + that.blue)
    def - (that: CSSColor): CSSColor = CSSColor(red - that.red, green - that.green, blue - that.blue)
    def + (that: Int): CSSColor = CSSColor(red + that, green + that, blue + that)
    private def colorHex(value: Int): String = value match {
      case x if x > 255 => "FF"
      case x if x < 0 => "00"
      case x => String.format("%02X", int2Integer(x))
    }
    def hex2Int(hex: String): Int = Integer.valueOf(hex, 16).intValue()
  }
  
  object CSSLength {
    def apply(value: Double, unit: Option[String]) = new CSSLength(value, unit)
  }

  class CSSLength(val value: Double, val unit: Option[String]) extends CSSValue {
    override def toString = {
      val rounded = value.round
      (if (rounded.toDouble == value) rounded.toString
       else value.toString) + unit.getOrElse("")
    }
    override def + (that: CSSValue) = CSSString(toString + that)
    def + (that: CSSLength): CSSLength = CSSLength(value + that.value, unit)
    def - (that: CSSLength): CSSLength = CSSLength(value - that.value, unit)
    def * (that: CSSLength): CSSLength = CSSLength(value * that.value, unit)
    def / (that: CSSLength): CSSLength = CSSLength(value / that.value, unit)
  }

  case class Op(val value: String) {
    override def toString = value
  }
}



case class Property(val name: String, val value: String) {
  override def toString = name + ":" + value + ";"
  def toString(constants: Map[String, String]): String = value match {
    case ConstantValueRegex(x) =>
      Property(name, ConstantCalculator.parse(ConstantCalculator.replaceConstants(x,constants))).toString
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
case class Script(val rules: List[RuleSet], val constants: List[Constant]) {
  override def toString = {
    val constantMap = constants.foldLeft(Map[String, String]()){(m,c) =>
      m ++ Map(c.name -> ConstantCalculator.parse(ConstantCalculator.replaceConstants(c.value,m)))} 
    rules.map(_.toString(constantMap, Nil)).mkString
  }
}

case class Constant(val name: String, val value: String)

class SassParsers extends RegexParsers {
  override val whiteSpace = "".r
  
  def script: Parser[Script] = ws ~> rep(constant) ~ rep(ruleset(0)) <~ ws ^^ {
    case c~r => Script(r, c)}
  
  def constant: Parser[Constant] = ("""!\S+""".r <~ sp ~ "=") ~ value <~ lf ^^ {
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
  
  val ws = """\s*""".r
  val ws1 = """\s+""".r
  val sp = """\ *""".r
  val sp1 = """\ +""".r
  val lf = """\s*(\n|\z)""".r  
}

object Sass extends SassParsers {
  def parseProperty(in: String) = parseAll(property(0), in)
  def parseProperties(in: String) = parseAll(properties(0), in)
  def parseNestedProperties(in: String) = parseAll(nestedProperties(0), in)
  def parseSelectors(in: String) = parseAll(selectors(0), in)
  def parse(in: String) = parseAll(script, in)
}

