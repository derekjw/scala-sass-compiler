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

  def expr: Parser[CSSValue] = cssValue~rep(op~cssValue) ^^ {
    case x~list => list.foldLeft(x)((r,n) => n match {case o~y => o(r,y)})
  }
  
  def cssValue: Parser[CSSValue] = parens | length | number | color | string | failure("Not a valid value")

  def parens: Parser[CSSValue] = sp~"("~sp~> expr <~sp~")"~sp
  
  def op: Parser[(CSSValue, CSSValue) => CSSValue] =
    sp~> opt( "+" ^^^ {(l: CSSValue, r: CSSValue) => l + r}
            | "-" ^^^ {(l: CSSValue, r: CSSValue) => l - r}
            | "*" ^^^ {(l: CSSValue, r: CSSValue) => l * r}
            | "/" ^^^ {(l: CSSValue, r: CSSValue) => l / r}
            ) <~sp ^^ {_.getOrElse((l: CSSValue, r: CSSValue) => l << r)}

  def number: Parser[CSSNumber] = decimalNumber ^^ {x => CSSNumber(x.toDouble)}

  def string: Parser[CSSString] = (quotedString | unquotedString) ^^ {CSSString(_)}
  
  def quotedString: Parser[String] = "\"" ~> opt("""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})+""".r) <~ "\"" ^^ {_.getOrElse("")} 
  def unquotedString: Parser[String] = """[^()"]\S*""".r
  
  def length: Parser[CSSLength] = number ~ unit ^^ {case x~u => CSSLength(x, u)}
  def unit: Parser[String] = sp~>"em"|"px"|"pt"|"%"
  
  def color: Parser[CSSColor] = (longColor | shortColor | wholeNumber) ^^ {CSSColor(_)} ^? {case Some(x) => x}
  
  def shortColor: Parser[String] = hexColor(3)
  def longColor: Parser[String] = hexColor(6)
  def hexColor(length: Int): Parser[String] = ("""#[0-9a-fA-F]"""+"{"+length+"}").r
  
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
  
  sealed abstract class CSSValue {
    def << (that: CSSValue): CSSString = CSSString(toString+" "+that)
    def + (that: CSSValue): CSSValue = CSSString(toString+that)
    def + (that: String): CSSString = CSSString(toString+that)
    def - (that: CSSValue): CSSValue = CSSString(toString+"-"+that)
    def * (that: CSSValue): CSSValue = CSSString(toString+"*"+that)
    def / (that: CSSValue): CSSValue = CSSString(toString+"/"+that)
  }

  case class CSSNumber(value: Double) extends CSSValue {
    override def toString = {
      val rounded = value.round
      if (rounded.toDouble == value) rounded.toString else value.toString
    }
    override def + (that: CSSValue): CSSValue = that match {
      case n: CSSNumber => this + n 
      case _ => super.+(that)
    }
    def + (that: CSSNumber): CSSNumber = CSSNumber(value + that.value)
    override def - (that: CSSValue): CSSValue = that match {
      case n: CSSNumber => this - n
      case _ => super.-(that)
    }
    def - (that: CSSNumber): CSSNumber = CSSNumber(value - that.value)
    override def * (that: CSSValue): CSSValue = that match {
      case n: CSSNumber => this * n
      case _ => super.*(that)
    }
    def * (that: CSSNumber): CSSNumber = CSSNumber(value * that.value)
    override def / (that: CSSValue): CSSValue = that match {
      case n: CSSNumber => this / n
      case _ => super./(that)
    }
    def / (that: CSSNumber): CSSNumber = CSSNumber(value / that.value)
  }
  
  case class CSSString(value: String) extends CSSValue {
    override def toString = value
  }
  
  object CSSColor {
    def apply(value:String): Option[CSSColor] = value match {
      case LongColor(red, green, blue)  => Some(new CSSColor(red,green,blue))
      case ShortColor(red, green, blue) => Some(new CSSColor(red+red,green+green,blue+blue))
      case _ => None}
    def apply(value:CSSValue): Option[CSSColor] = CSSColor(value.toString)
    def apply(number:CSSNumber): CSSColor = {
      val rgb = number.value.toInt
      CSSColor(rgb,rgb,rgb)
    }
    def apply(red: Int, green: Int, blue: Int) = new CSSColor(red, green, blue)
    def unapply(in: CSSColor): Option[(Int, Int, Int)] = Some((in.red, in.green, in.blue))
  }

  class CSSColor(val red: Int, val green: Int, val blue: Int) extends CSSValue {
    def this(strRed: String, strGreen: String, strBlue: String) =
      this(Integer.valueOf(strRed, 16).intValue(),
           Integer.valueOf(strGreen, 16).intValue(),
           Integer.valueOf(strBlue, 16).intValue())
    override def toString = "#" + colorHex(red) + colorHex(green) + colorHex(blue)
    override def + (that: CSSValue): CSSValue = that match {
      case CSSColor(r,g,b) => CSSColor(red + r, green + g, blue + b)
      case n: CSSNumber => this + CSSColor(n)
      case _ => super.+(that)
    }
    override def - (that: CSSValue): CSSValue = that match {
      case CSSColor(r,g,b) => CSSColor(red - r, green - g, blue - b)
      case n: CSSNumber => this - CSSColor(n)
      case _ => super.-(that)
    }
    private def colorHex(value: Int): String = value match {
      case x if x > 255 => "FF"
      case x if x < 0 => "00"
      case x => String.format("%02X", int2Integer(x))
    }
    def hex2Int(hex: String): Int = Integer.valueOf(hex, 16).intValue()
  }
  
  object CSSLength {
    def apply(n: Double, unit: String): CSSLength = CSSLength(CSSNumber(n), unit)
    def apply(n: CSSNumber, unit: String): CSSLength = new CSSLength(n, unit)
    def unapply(in: CSSLength): Option[(CSSNumber, String)] = Some(in.value, in.unit)
  }

  class CSSLength(val value: CSSNumber, val unit: String) extends CSSValue {
    override def toString =
      value.toString + unit
    override def + (that: CSSValue): CSSValue = that match {
      case CSSLength(x,_) => CSSLength(value + x, unit)
      case x => super.+(x)
    }
    override def - (that: CSSValue): CSSValue = that match {
      case CSSLength(x,_) => CSSLength(value - x, unit)
      case x => super.-(x)
    }
    override def * (that: CSSValue): CSSValue = that match {
      case CSSLength(x,_) => CSSLength(value * x, unit)
      case x => super.*(x)
    }
    override def / (that: CSSValue): CSSValue = that match {
      case CSSLength(x,_) => CSSLength(value / x, unit)
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
