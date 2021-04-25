/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  val ALPHANUM   = CharPredicate.AlphaNum
  val ALPHANUM$  = CharPredicate.AlphaNum ++ '$'
  val ALPHANUM$_ = CharPredicate.AlphaNum ++ $_

  val KEYCHAR  = CharPredicate(":;=#@\u21d2\u2190")
  val KEYCHAR2 = CharPredicate("-:%")
}
