package ca.fyrie.parsing

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.immutable.PagedSeq
import CharArrayReader.EofCh

class CharParsers extends Parsers {
  type Elem = Char

  def letter = elem("letter", _.isLetter)

  def digit = elem("digit", _.isDigit)

  def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch !=)))

  def ident: Parser[String] = rep1(letter|'_') ^^ (_.mkString)

  def num = intPart ~ opt(fracPart) ~ opt(expPart) ^^ { case i ~ f ~ e =>
    i + optString(".", f) + optString("", e)
  }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ { case e ~ s ~ d =>
    e + optString("", s) + d.mkString("")
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }
  
  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
    |'\\' ~ '\\' ^^^ "\\"
    |'\\' ~ '/'  ^^^ "/"
    |'\\' ~ 'b'  ^^^ "\b"
    |'\\' ~ 'f'  ^^^ "\f"
    |'\\' ~ 'n'  ^^^ "\n"
    |'\\' ~ 'r'  ^^^ "\r"
    |'\\' ~ 't'  ^^^ "\t"
    |'\\' ~> 'u' ~> unicodeBlock)
  
  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))
  
  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }

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

  def sp: Parser[String] = rep(' ') ^^ {_.mkString}
  def sp1: Parser[String] = rep1(' ') ^^ {_.mkString}
  def lf: Parser[String] = rep1(sp ~ "\n") ^^ {_.mkString}
  def ws: Parser[String] = rep("\n"|"\t"|" ") ^^ {_.mkString}
  def eof: Parser[Char] = EofCh

  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = phrase(p)(in)
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = phrase(p)(new CharSequenceReader(in))
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = phrase(p)(new PagedSeqReader(PagedSeq.fromReader(in)))
}
