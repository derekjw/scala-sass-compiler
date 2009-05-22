package ca.nebvin.utils

import org.specs._
import org.specs.runner.JUnit4

import scala.util.parsing.combinator._
import ca.nebvin.utils._

class SassTest extends JUnit4(SassSpec)

object SassSpec extends Specification {
  "sass parser" should {
    "parse a simple script" in {
      val result = Sass.parse("div#main a, .sidebar p\n  :font-size 10px\n  :font-color black\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a, .sidebar p { font-size:10px; font-color:black; }\n")
    } 
    "parse a simple script with 2 selector lists" in {
      val result = Sass.parse("div#main a, .sidebar p\n  :font-size 10px\n  :font-color black\n\nh1\n  :font-size 1em\n")
      result.successful mustBe true
    } 
    "parse a script with child rulesets" in {
      val result = Sass.parse("div#main a, .sidebar p\n  :font-size 10px\n  :font-color black\n  h1\n    span, div\n      :font-family Arial")
      result.successful mustBe true
      result.get must beEqualTo("div#main a, .sidebar p { font-size:10px; font-color:black; }\ndiv#main a h1 span, div#main a h1 div, .sidebar p h1 span, .sidebar p h1 div { font-family:Arial; }\n")
    }
    "parse a script with nested properties" in {
      val result = Sass.parse("div#main a, .sidebar p\n  :font\n    :size 10px\n    :color black\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a, .sidebar p { font-size:10px; font-color:black; }\n")
    } 
    "parse a script with parent references in selectors" in {
      val result = Sass.parse("div#main a, .sidebar p\n  :font\n    :size 10px\n  &:after\n    :margin-top 10px\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a, .sidebar p { font-size:10px; }\ndiv#main a:after, .sidebar p:after { margin-top:10px; }\n")
    } 
    "parse a script with simple constants" in {
      val result = Sass.parse("!test_color = black\ndiv#main a, .sidebar p\n  :font-size 10px\n  :font-color = !test_color\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a, .sidebar p { font-size:10px; font-color:black; }\n")
    } 
    "parse a script with calculated constants" in {
      val result = Sass.parse("!test_size = 10px\ndiv#main a\n  :font-size = !test_size + 13px\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a { font-size:23px; }\n")
    } 
    "parse a script with calculated color constants" in {
      val result = Sass.parse("!test_color = #123\n!other_color = !test_color + 20\ndiv#main a\n  :background-color = !other_color\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a { background-color:#253647; }\n")
    } 
    "parse a script with complex calculated constants" in {
      val result = Sass.parse("!partial_border = 1px \"solid\"\n!test_color = #123\n!other_color = !test_color - 1\n!border = !partial_border (!other_color + #111)\ndiv#main a\n  :border = !border\ndiv#other\n  :border = 2px dashed (!other_color + #222)\n")
      result.successful mustBe true
      result.get must beEqualTo("div#main a { border:1px solid #213243; }\ndiv#other { border:2px dashed #324354; }\n")
    } 
  }
}
