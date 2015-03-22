package scalaparser

import org.parboiled2._

import scala.annotation.switch
import scalaparser.L0_Basics._

object L1_KeywordsAndOperators extends SimpleParser {
  import CharacterClasses._
  import L0_Basics._

  val Operator = rule { !SymbolicKeyword ~ OpChar.+ }

  val Keyword = rule( AlphaKeyword | SymbolicKeyword )

  val underscore  = Key("_")
  val `abstract`  = Key("abstract")
  val `case`      = Key("case")
  val `catch`     = Key("catch")
  val `class`     = Key("class")
  val `def`       = Key("def")
  val `do`        = Key("do")
  val `else`      = Key("else")
  val `extends`   = Key("extends")
  val `final`     = Key("final")
  val `finally`   = Key("finally")
  val `for`       = Key("for")
  val `forSome`   = Key("forSome")
  val `if`        = Key("if")
  val `implicit`  = Key("implicit")
  val `import`    = Key("import")
  val `lazy`      = Key("lazy")
  val `macro`     = Key("macro")
  val `match`     = Key("match")
  val `new`       = Key("new")
  val `object`    = Key("object")
  val `override`  = Key("override")
  val `package`   = Key("package")
  val `private`   = Key("private")
  val `protected` = Key("protected")
  val `return`    = Key("return")
  val `sealed`    = Key("sealed")
  val `super`     = Key("super")
  val `this`      = Key("this")
  val `throw`     = Key("throw")
  val `trait`     = Key("trait")
  val `try`       = Key("try")
  val `type`      = Key("type")
  val `val`       = Key("val")
  val `var`       = Key("var")
  val `while`     = Key("while")
  val `with`      = Key("with")
  val `yield`     = Key("yield")

  val `<%` = SymbolicKey("<%")
  val `>:` = SymbolicKey(">:")
  val `<:` = SymbolicKey("<:")
  val `=>` = rule( SymbolicKey("=>") | SymbolicKey('⇒') )
  val `<-` = rule( SymbolicKey("<-") | SymbolicKey('←') )
  val `:`  = SymbolicKey(':')
  val `=`  = SymbolicKey('=')
  val `@`  = SymbolicKey('@')
  val `#`  = SymbolicKey("#")

  val Null = rule(RawKey("null"))
  val True = rule(RawKey("true"))
  val False = rule(RawKey("false"))

  // keyword-like patterns (not really keywords though)
  val `_*` = rule( underscore ~ WL ~ "*" )
  val `}` = rule( Semis.? ~ WL ~ '}' )
  val `{` = rule( WL ~ '{' ~ Semis.? )

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def Key(s: String) = rule( WL ~ RawKey(s) )
  private val RawKey = rule[String]() ( s => s ~ !AlphaNum$_ )
  private def SymbolicKey(c: Char) = rule( WL ~ c ~ !OpChar )
  private def SymbolicKey(s: String) = rule( WL ~ s ~ !OpChar )

  private val SymbolicKeyword = rule { ("=>" | KEYCHAR | '<' ~ KEYCHAR2 | ">:") ~ !OpChar }

  private val AlphaKeyword = rule {
    run {
      // TODO: simplify when https://github.com/sirthias/parboiled2/issues/115 is done
      state.cursorChar match {
        case 'a' => str("abstract")
        case 'c' => "case" | "catch" | "class"
        case 'd' => "def" | "do"
        case 'e' => "else" | "extends"
        case 'f' => "false" | "final" ~ "ly".? | "forSome" | "for"
        case 'i' => "if" | "implicit" | "import"
        case 'l' => str("lazy")
        case 'm' => str("match")
        case 'n' => "new" | "null"
        case 'o' => "object" | "override"
        case 'p' => "package" | "private" | "protected"
        case 'r' => str("return")
        case 's' => "sealed" | "super"
        case 't' => "this" | "throw" | "trait" | "try" | "true" | "type"
        case 'v' => "val" | "var"
        case 'w' => "while" | "with"
        case 'y' => str("yield")
        case '_' => ANY
        case _ => MISMATCH
      }
    } ~ !AlphaNum$_
  }
}
