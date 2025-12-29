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

import org.parboiled2.*

class ScalaParser(val input: ParserInput)
    extends Parser with WhitespaceStringsAndChars with L0_Basics with L1_KeywordsAndOperators with L2_Identifiers
    with L3_Literals with L4_Types with L4_Xml with L5_Exprs with L6_TopLevel
