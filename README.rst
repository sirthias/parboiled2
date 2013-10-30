*********************************************************
`parboiled2`_: a Macro-Based PEG Parser Generator for Scala
*********************************************************

.. _parboiled2: https://github.com/sirthias/parboiled2

Motivation
==========

Grammar-based parsing of text data is a ubiquitous problem in real-world applications. One popular technique for implementing parsers is parser combinators. However, even though it's comparatively easy to build a parser with combinators the end-result is logic that essentially "interprets" the grammar rules against the input, which is rather slow. Hand-writing a parser can yield a much faster implementation but is tedious and error-prone.

It is possible to implement a parser defined via an embedded DSL that is translated into runnable code by the host language compiler at compile time. The macro support introduced with Scala 2.10 provides the required and sufficient infrastructure for such an endeavor.

*parboiled2* is a next version of `parboiled`_ library. *parboiled2* is a Scala library that provides lightweight and modular way to build a parser of arbitrary input based on `parsing expression grammars <http://en.wikipedia.org/wiki/Parsing_expression_grammar>`_ (PEGs). *parboiled2* uses Scala 2.10 macros to implement a compile-time generator for highly efficient PEG parsers. Rule definition is based on the `parboiled DSL`_ embedded in Scala.


.. _parboiled: http://parboiled.org

.. _parboiled DSL: https://github.com/sirthias/parboiled/wiki/Rule-Construction-in-Scala

Quick Start
===========

Launching Simple Parser
-----------------------

First basic task of parsing is to determine if input string is conformed to target PEG. For example, having a PEG described in *parboiled2* `DSL`_ as follows

.. code-block:: Scala

  import org.parboiled2._
  class SimpleParser (val input: ParserInput) extends Parser {
    def InputLine = rule { "ab" | "cd" }
  }

This ``SimpleParser`` will just tell whether an input string is ``"ab"`` or ``"cd"``, or fail otherwise. To check it at runtime:

.. code-block:: bash

  scala> val inputLine = "cd"
  inputLine: String = cd

  scala> val simpleParser = new org.parboiled2.examples.v2.SimpleParser(inputLine)
  simpleParser: org.parboiled2.examples.v2.SimpleParser = org.parboiled2.examples.v2.SimpleParser@46431d8c

  scala> simpleParser.run(_.InputLine)
  res0: simpleParser.Result[shapeless.HNil] = Right(HNil)

Note that type of ``res0`` is ``Right``. It means the given string ``"cd"`` is conformed input grammar.

Computation Process
-------------------

How does *parboiled2* compute whether input string is matched or not? Being compiled ``SimpleParser.InputLine`` code will be transformed to a piece equivalent to a following one:

.. code-block:: Scala

  def InputLine = {
    val mark = this.mark
    val left = {
      val string = "ab"
      var ix = 0
      while (ix < string.length && this.__nextChar() == string.charAt(ix)) ix += 1
      Rule(ix == string.length())
    }

    if (left.matched)
      left
    else {
      this.reset(mark)
      {
        val string = "cd"
        var ix = 0
        while (ix < string.length && this.__nextChar() == string.charAt(ix)) ix += 1
        Rule(ix == string.length())
      }
    }
  }

Minimum of intermediate object. Minimum of method calls. Run-time effective imperative-style code is where effectiveness of *parboiled2* comes from.

Error Reporting
---------------

Parsing error message will be returned if input string does not conform to target grammar. To help to understand why parser failed, error message contains all necessary information. For example, matching ``"bb"`` against ``SimpleParser.InputLine`` will produce an error of following shape:

.. code-block:: Scala

  scala> val inputLine = "bb"
  inputLine: String = bb

  scala> val simpleParser = new org.parboiled2.examples.v2.SimpleParser(inputLine)
  simpleParser: org.parboiled2.examples.v2.ABCParser = org.parboiled2.examples.v2.SimpleParser@5b264dee

  scala> simpleParser.run(_.InputLine)
  res0: simpleParser.Result[shapeless.HNil] = Left(ParseError(Position(0,1,1),Vector(RuleStack(Vector(LiteralString(ab,), FirstOf(InputLine))), RuleStack(Vector(LiteralString(cd,), FirstOf(InputLine))))))

``Left`` means that there was a parsing error. ``ParseError`` instance contains ``Position`` and ``Seq[RuleStack]``. ``Position`` is an ``index``, ``line`` and ``column`` of input start position that failed all possible rules. ``index`` is zero-based, ``line`` and ``column`` are one-based. ``Seq[RuleStack]`` contains all these rules stacks (top of each stack is at ``Vector(0)``) that failed. You can format ``ParseError`` data structure in way most appropriate for your application. *parboiled2* provides ``ErrorUtils.formatError`` that will produce a string:

.. code-block:: bash

  scala> org.parboiled2.ErrorUtils.formatError(inputLine, res0.left.get)

  res1: String =
  expected
  RuleStack(Vector(FirstOf(InputLine), LiteralString(ab,)))

  RuleStack(Vector(FirstOf(InputLine), LiteralString(cd,)))
   (line 1, column 1):
  bb
  ^

Value-Stack
-----------

Type-Driven Implementation
^^^^^^^^^^^^^^^^^^^^^^^^^^

*parboiled2* can produce *abstract syntax tree* as a side effect of interaction with ``value-stack``. ``value-stack`` is inner data structure that is referentially invisible from an application. Only ``value-stack`` actions embedded in rules can check and change it: ``capture``, ``~>``, ``push`` and ``~?``. 

Major part of ``value-stack`` manipulations errors could be caught at compile time. It is provided by typing of ``Rule``. ``Rule`` is defined as follows:

.. code-block:: Scala

  sealed abstract class Rule[-I <: HList, +O <: HList]

It is characterized by consuming a certain number of elements from the ``value-stack``. Its types are captured by the ``HList`` type parameter ``I`` for "Input". And itself pushing a certain number of elements onto the ``value-stack``. Its types in turn are captured by the ``HList`` type parameter `O` for "Output". 

For example, rule of type ``Rule[String :: HNil, Int :: HNil]`` will pop ``String`` from ``value-stack`` and push a value of type ``Int``. Special case is so-called reduction rules that pop two or more values and push a single one.

Note that ``Rule``'s ``I`` and ``O`` types grow from left to right. Means that for type ``Int :: String :: HNil`` value of type ``Int`` was pushed first, and after that value of type ``String``.

Actions
^^^^^^^

``capture``
"""""""""""

``capture`` action rule is defined as

.. code-block:: Scala

  def capture(r: Rule[I, O]): Rule[I, O :: String]

If ``r`` matched then part of matched input is pushed to ``value-stack``. Otherwise it fails matching. For example, matching 

.. code-block:: Scala

  rule { capture(oneOrMore("a")) }

against ``"a"`` will cause ``value-stack`` to be ``"a" :: HNil``. "aa" -- ``"aa" :: HNil``. And ``"b"`` will fail.

``~>``
""""""

``~>`` accepts a function of ``n`` arguments on right side. Then provided function is called with values substituted by ``n`` popped values from ``value-stack``. Result of a function is pushed back to ``value-stack``. For example,

.. code-block:: Scala

  rule { capture("0" - "9") ~> ((x: String) => x.toInt) }

``-`` is a syntactic sugar for a rule that matches first of character in provided range. After string representation of a digit is matched, captured string is pushed on ``value-stack``. ``~>`` pops it and passes to lambda-function where it is converted to ``Int``. As a result ``value-stack`` state is ``7 :: HNil`` for ``"7"`` input. 

Note that type of a function is statically typed: It can be only of type ``(String) => A``. Rule type in turn will be of type ``Rule[HNil, Int :: HNil]``.

Underscored functions are also valid. This is equivalent to previous example:

.. code-block:: Scala

    rule { capture("0" - "9") ~> (_.toInt) }

Reduction Rules
^^^^^^^^^^^^^^^

``~>`` together with varying-matching operators (``zeroOrMore``, ``oneOrMore`` and ``Optional``) gives possibility of ``value-stack`` reductions. Consider grammar as follows:

.. code-block:: Scala

    def Digit = rule { capture("0" - "9") ~> (_.toInt) }
    rule { Digit ~ zeroOrMore(Digit ~> ((_: Int) + _)) }

Each ``Digit`` is pushing ``Int`` onto ``value-stack``. When ``Digit`` inside ``zeroOrMore`` is matched there are two ``Int`` s on ``value-stack``. Both ``Int`` s are popping by ``~>`` and passing to a lambda that sums them. And so forth. As a result there is a result of summing of input digits on ``value-stack``: ``"123"`` causes ``6 :: HNil``.

Abstract Syntax Tree
^^^^^^^^^^^^^^^^^^^^

Result of ``~>``'s functions is not limited to predefined types (like ``Int``):

.. code-block:: Scala

    case class CapturedString(s: String)
    rule { capture("a") ~> ((x: String) => CapturedString(x)) }
    rule { capture("a") ~> (CapturedString(_)) }
    rule { capture("a") ~> CapturedString }

All three forms are equivalent. Matching against ``"a"`` causes ``value-stack`` to have state ``CapturedString("a") :: HNil``.

``push``
^^^^^^^^

``push`` rule just pushes provided value on ``value-stack``.

.. code-block:: Scala

    rule { "true" ~ push(true) }

This rule pushes ``true: Boolean`` right after ``"true"`` string is matched.

``~?``
^^^^^^

Operator provides semantic predicates during parsing. Consider a grammar:

.. code-block:: Scala

    def Digit = rule { capture("0" - "9") }
    def Digits = rule { oneOrMore(Digit) ~> (_.toInt) }
    def LargeNumber = rule { Digits ~? (_ > 1000) }

``~?`` does not change ``value-stack``. The type of ``LargeNumber`` is same as type of ``Digits``. ``~?`` checks if top value of ``value-stack`` is true for provided predicate. It matches if predicate returns ``true``, and fails otherwise. That grammar allows only a number larger than 1000 to be on ``value-stack``.

DSL
===

Correspondence of PEG operators [bford_] and *parboiled2* DSL primitive expressions (`a` and `b` are parsing expressions):

+----------------+--------------+-----------------------+
| Operator       | PEG notation | *parboiled2* notation |
+================+==============+=======================+
| Sequence       | a ~ b        | a ~ b                 |
+----------------+--------------+-----------------------+
| Ordered Choice | a | b        | a | b                 |
+----------------+--------------+-----------------------+
| Zero-or-more   | a *          | zeroOrMore(a)         |
+----------------+--------------+-----------------------+
| One-or-more    | a +          | oneOrMore(a)          |
+----------------+--------------+-----------------------+
| Optional       | a ?          | optional(a)           |
+----------------+--------------+-----------------------+
| And-predicate  | & a          | &(a)                  |
+----------------+--------------+-----------------------+
| Not-predicate  | ! a          | ! a                   |
+----------------+--------------+-----------------------+

`Examples <https://github.com/sirthias/parboiled2/tree/master/examples/src/main/scala/org/parboiled2/examples>`_ of using `DSL`.

.. _bford: http://pdos.csail.mit.edu/papers/parsing:popl04.pdf
