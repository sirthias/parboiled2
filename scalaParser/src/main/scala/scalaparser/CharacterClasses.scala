package scalaparser

import org.parboiled2.CharPredicate

object CharacterClasses {

  val DIGIT = CharPredicate.Digit

  val HEXDIGIT = CharPredicate.HexDigit

  val `Ee` = CharPredicate("Ee")

  val `Ll` = CharPredicate("Ll")

  val `+-` = CharPredicate("+-")

  val DQLF = CharPredicate("\"\n")
  val DQBS = CharPredicate("\"\\")

  val `FfDd` = CharPredicate("FfDd")

  val ESCAPEE = CharPredicate("""btnfr'\"""")

  val WSCHAR = CharPredicate(" \t")

  val OPCHAR = CharPredicate("""!#$%&*+-/:<=>?@\^|~""")

  val $_ = CharPredicate("$_")

  val LOWER = CharPredicate.LowerAlpha

  val LOWER$_ = CharPredicate.LowerAlpha ++ $_

  val UPPER = CharPredicate.UpperAlpha

  val ALPHA = CharPredicate.Alpha

  val ALPHANUM = CharPredicate.AlphaNum
  val ALPHANUM$ = CharPredicate.AlphaNum ++ '$'
  val ALPHANUM$_ = CharPredicate.AlphaNum ++ $_

  val KEYCHAR = CharPredicate(":;=#@\u21d2\u2190")
  val KEYCHAR2 = CharPredicate("-:%")
}
