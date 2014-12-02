package scalaparser

import scala.language.implicitConversions
import org.parboiled2._

trait L4_Core { this: Parser with WhitespaceStringsAndChars
  with L0_Basics
  with L1_KeywordsAndOperators
  with L2_Identifiers
  with L3_Literals =>

  import CharacterClasses._

  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `_*` = rule( `_` ~ "*" )
  def `}` = rule( Semis.? ~ '}' )
  def `{` = rule( '{' ~ Semis.? )

  def WLId = rule( WL ~ Id )
  def WLVarId = rule( WL ~ VarId )
  def WLLiteral = rule( WL ~ Literal )
  def WLNewline = rule( WL ~ Newline )
  def Semi = rule( WS ~ (';' | Newline.+) )
  def Semis = rule( Semi.+ )

  def QualId = rule( WL ~ WLId.+('.') )
  def WLIds = rule( WLId.+(',') )

  def NotNewline: Rule0 = rule( &( WS ~ !Newline ) )
  def OneNLMax: Rule0 = {
    def WSChar = rule( WSCHAR.* )
    def ConsumeComments = rule( (WSChar ~ Comment ~ WSChar ~ Newline).* )
    rule( WS ~ Newline.? ~ ConsumeComments ~ NotNewline )
  }
  def StableId: Rule0 = {
    def ClassQualifier = rule( '[' ~ WLId ~ ']' )
    def ThisSuper = rule( `this` | `super` ~ ClassQualifier.? )
    rule( (WLId ~ '.').* ~ ThisSuper ~ ('.' ~ WLId).* | WLId ~ ('.' ~ WLId).* )
  }

}