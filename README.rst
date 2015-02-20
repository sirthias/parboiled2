**parboiled2**  |--| A Macro-Based PEG Parser Generator for Scala 2.10.3+

.. contents:: Contents of this Document


Introduction
============

*parboiled2* is a Scala 2.10.3+ library enabling lightweight and easy-to-use, yet powerful, fast and elegant parsing of
arbitrary input text. It implements a macro-based parser generator for `Parsing Expression Grammars`_ (PEGs), which
runs at compile time and translates a grammar rule definition (written in an internal Scala DSL) into corresponding JVM
bytecode.

PEGs are an alternative to `Context-Free Grammars`_ (CFGs) for formally specifying syntax, they make a good replacement
for regular expressions and have some advantages over the "traditional" way of building parsers via CFGs (like not
needing a separate lexer/scanner phase).

*parboiled2* is the successor of `parboiled 1.x`_ , which provides a similar capability (for Scala as well as Java) but
does not actually *generate* a parser. Rather `parboiled 1.x`_ interprets a rule tree structure (which is also created
via an internal DSL) against the input, which results in a much lower parsing performance.
For more info on how `parboiled 1.x`_ and *parboiled2* compare see `parboiled2 vs. parboiled 1.x`_.
You might also be interested in reading about `parboiled2 vs. Scala Parser Combinators`_ and
`parboiled2 vs. Regular Expressions`_.

.. _PEG:
.. _Parsing Expression Grammars: http://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _Context-Free Grammars: http://en.wikipedia.org/wiki/Context-free_grammar
.. _parboiled 1.x: http://parboiled.org


Features
========

* Concise, flexible and type-safe DSL for expressing parsing logic
* Full expressive power of `Parsing Expression Grammars`_, for effectively dealing with most real-world parsing needs
* Excellent reporting of parse errors
* Parsing performance comparable to hand-written parsers
* Easy to learn and use (just one parsing phase (no lexer code required), rather small API)
* Light-weight enough to serve as a replacement for regular expressions (also strictly more powerful than regexes)


Installation
============

The artifacts for *parboiled2* live on `Maven Central`_ and can be tied into your SBT-based Scala project like this:

.. code:: Scala

    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

The latest released version is **2.1.0**. It is available for Scala 2.10.3+ as well as Scala 2.11.

*parboiled2* has only one single dependency that it will transitively pull into your classpath: shapeless_
(currently version 2.1.0).

**Note:** If your project also uses ``"io.spray" %% "spray-routing"``
you'll need to change this to ``"io.spray" %% "spray-routing-shapeless2"`` in order for
your project to continue to build since the "regular" spray builds use shapeless 1.x.

Once on your classpath you can use this single import to bring everything you need into scope:

.. code:: Scala

    import org.parboiled2._

There might be potentially newer snapshot builds available in the *sonatype snapshots* repository located at:
https://oss.sonatype.org/content/repositories/snapshots/

You can find the latest ones here:
https://oss.sonatype.org/content/repositories/snapshots/org/parboiled/parboiled_2.10/ (Scala 2.10) and
https://oss.sonatype.org/content/repositories/snapshots/org/parboiled/parboiled_2.11/ (Scala 2.11)

.. _Maven Central: http://search.maven.org/
.. _shapeless: https://github.com/milessabin/shapeless


Example
=======

This is what a simple *parboiled2* parser looks like:

.. code:: Scala

    import org.parboiled2._

    class Calculator(val input: ParserInput) extends Parser {
      def InputLine = rule { Expression ~ EOI }

      def Expression: Rule1[Int] = rule {
        Term ~ zeroOrMore(
          '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _))
      }

      def Term = rule {
        Factor ~ zeroOrMore(
          '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _))
      }

      def Factor = rule { Number | Parens }

      def Parens = rule { '(' ~ Expression ~ ')' }

      def Number = rule { capture(Digits) ~> (_.toInt) }

      def Digits = rule { oneOrMore(CharPredicate.Digit) }
    }

    new Calculator("1+1").InputLine.run() // evaluates to `scala.util.Success(2)`

This implements a parser for simple integer expressions like ``1+(2-3*4)/5`` and runs the actual calculation in-phase
with the parser. If you'd like to see it run and try it out yourself check out `Running the Examples`_.


Quick Start
===========

A *parboiled2* parser is a class deriving from ``org.parboiled2.Parser``, which defines one abstract member:

.. code:: Scala

    def input: ParserInput

holding the input for the parsing run. Usually it is best implemented as a ``val`` parameter in the constructor
(as shown in the Example_ above). As you can see from this design you need to (re-)create a new parser instance for
every parsing run (parser instances are very lightweight).

The "productions" (or "rules") of your grammar are then defined as simple methods, which in most cases consist of a
single call to the ``rule`` macro whose argument is a `DSL expression`_ defining what input the rule is to match and
what actions_ to perform.

In order to run your parser against a given input you create a new instance and call ``run()`` on the top-level rule,
e.g:

.. code:: Scala

    val parser = new MyParser(input)
    parser.topLevelRule.run() // by default returns a ``scala.util.Try``

For more info on what options you have with regard to accessing the results of a parsing run check out the section
on `Access to Parser Results`_.

.. _DSL expression: `The Rule DSL`_
.. _actions: `Parser Actions`_


How the Parser matches Input
============================

PEG_ parsers are quite easy to understand as they work just like most people without a lot of background in parsing
theory would build a parser "by hand": recursive-descent with backtracking. They have only one parsing phase (not two,
like mosts parsers produced by traditional parser generators like ANTLR_), do not require any look-ahead and perform
quite well in most real-world scenarios (although they *can* exhibit exponential runtime for certain pathological
languages and inputs).

A PEG_ parser consists of a number of rules that logically form a "tree", with one "root" rule at the top calling zero
or more lower-level rules, which can each call other rules and so on. Since rules can also call themselves or any of
their parents the rule "tree" is not really a tree but rather a potentially cyclic directed graph, but in most cases the
tree structure dominates, which is why its useful to think of it as a tree with potential cycles.

When a rule is executed against the current position in an input buffer it applies its specific matching logic to the
input, which can either succeed or fail. In the success case the parser advances the input position (the *cursor*) and
potentially executes the next rule. Otherwise, when the rule fails, the cursor is reset and the parser backtracks in
search of another parsing alternative that might succeed.

For example consider this simple *parboiled2* rule:

.. code:: Scala

    def foo = rule { 'a' ~ ('b' ~ 'c' | 'b' ~ 'd') }

When this rule is confronted with the input ``abd`` the parser matches the input in these steps:

1. Rule ``foo`` starts executing, which calls its first sub-rule ``'a'``. The cursor is at position 0.
2. Rule ``'a'`` is executed against input position 0, matches (succeeds) and the cursor is advanced to position 1.
3. Rule ``'b' ~ 'c' | 'b' ~ 'd'`` starts executing, which calls its first sub-rule ``'b' ~ 'c'``.
4. Rule ``'b' ~ 'c'`` starts executing, which calls its first sub-rule ``'b'``.
5. Rule ``'b'`` is executed against input position 1, matches (succeeds) and the cursor is advanced to position 2.
6. Rule ``'c'`` is executed against input position 2 and mismatches (fails).
7. Rule ``'b' ~ 'c' | 'b' ~ 'd'`` notices that its first sub-rule has failed, resets the cursor to position 1 and
   calls its 2nd sub-rule ``'b' ~ 'd'``.
8. Rule ``'b' ~ 'd'`` starts executing, which calls its first sub-rule ``'b'``.
9. Rule ``'b'`` is executed against input position 1, matches and the cursor is advanced to position 2.
10. Rule ``'d'`` is executed against input position 2, matches and the cursor is advanced to position 3.
11. Rule ``'b' ~ 'd'`` completes successfully, as its last sub-rule has succeeded.
12. Rule ``'b' ~ 'c' | 'b' ~ 'd'`` completes successfully, as one of its sub-rules has succeeded.
13. Rule ``foo`` completes execution successfully, as its last sub-rule has succeeded.
    The whole input "abd" was matched and the cursor is left at position 3 (after the last-matched character).

.. _ANTLR: http://www.antlr.org/


The Rule DSL
============

In order to work with *parboiled2* effectively you should understand the core concepts behind its rule DSL, mainly
the "Value Stack" and how *parboiled2* encodes value stack operations in the Scala type system.


Rule Types and the Value Stack
------------------------------

Apart from the input buffer and the cursor the parser manages another important structure: the "Value Stack".
The value stack is a simple stack construct that serves as temporary storage for your `Parser Actions`_. In many cases
it is used for constructing an AST_ during the parsing run but it can also be used for "in-phase" computations
(like in the Example_ above) or for any other purpose.

When a rule of a *parboiled2* parser executes it performs any combination of the following three things:

- match input, i.e. advance the input cursor
- operate on the value stack, i.e. pop values off and/or push values to the value stack
- perform side-effects

Matching input is done by calling `Basic Character Matching`_ rules, which do nothing but match input and advance
the cursor. Value stack operations (and other potential side-effects) are performed by `Parser Actions`_.

It is important to understand that rules in *parboiled2* (i.e. the rule methods in your parser class) do not directly
return some custom value as a method result. Instead, all their consuming and producing values happens as side-effects
to the value stack. Thereby the way that a rule interacts with value stack is encoded in the rule's type.

This is the general definition of a *parboiled2* rule:

.. code:: Scala

    class Rule[-I <: HList, +O <: HList]

This can look scary at first but is really quite simple. An ``HList`` is defined by shapeless_ and is essentially a type
of list whose element number and element types are statically known at compile time. The ``I`` type parameter on
``Rule`` encodes what values (the number and types) the rule pops off the value stack and the ``O`` type parameter
encodes what values (the number and types) the rule then pushes onto the value stack.

Luckily, in most cases, you won't have to work with these types directly as they can either be inferred or you can use
one of these predefined aliases:

.. code:: Scala

    type Rule0 = RuleN[HNil]
    type Rule1[+T] = RuleN[T :: HNil]
    type Rule2[+A, +B] = RuleN[A :: B :: HNil]
    type RuleN[+L <: HList] = Rule[HNil, L]
    type PopRule[-L <: HList] = Rule[L, HNil]

Here is what these type aliases denote:

Rule0
    A rule that neither pops off nor pushes to the value stack, i.e. has no effect on the value stack whatsoever.
    All `Basic Character Matching`_ rules are of this type.

Rule1[+T]
    Pushes exactly one value of type ``T`` onto the value stack. After ``Rule0`` this is the second-most frequently
    used rule type.

Rule2[+A, +B]
    Pushes exactly two values of types ``A`` and ``B`` onto the value stack.

RuleN[+L <: HList]
    Pushes a number of values onto the value stack, which correspond to the given ``L <: HList`` type parameter.

PopRule[-L <: HList]
    Pops a number of values off the value stack (corresponding to the given ``L <: HList`` type parameter) and does
    not produce any new value itself.

The rule DSL makes sure that the rule types are properly assembled and carried through your rule structure as you
combine `Basic Character Matching`_  with `Rule Combinators and Modifiers`_ and `Parser Actions`_, so
as long as you don't write any logic that circumvents the value stack your parser will be completely type-safe and
the compiler will be able to catch you if you make mistakes by combining rules in an unsound way.

.. _AST: http://en.wikipedia.org/wiki/Abstract_syntax_tree


Basic Character Matching
------------------------

The following basic character matching rules are the only way to cause the parser to match actual input and
"make progress". They are the "atomic" elements of the rule DSL which are then used by the
`Rule Combinators and Modifiers`_ to form higher-level rules.

----

implicit def ch(c: Char): Rule0
    ``Char`` values can be directly used in the rule DSL and match themselves. There is one notable case where you will
    have to use the explicit ``ch`` wrapper: You cannot use the ``|`` operator directly on chars as it denotes the
    built-in Scala binary "or" operator defined on numeric types (``Char`` is an unsigned 16-bit integer).
    So rather than saying ``'a' | 'b'`` you will have to say ``ch('a') | 'b'``.

----

implicit def str(s: String): Rule0
    ``String`` values can be directly used in the rule DSL and match themselves.

----

implicit def predicate(p: CharPredicate): Rule0
    You can use ``org.parboiled2.CharPredicate`` values directly in the rule DSL. ``CharPredicate`` is an efficient
    implementation of character sets and already comes with a number pre-defined character classes like
    ``CharPredicate.Digit`` or ``CharPredicate.LowerHexLetter``.

----

implicit def valueMap[T](m: Map[String, T]): R
    Values of type ``Map[String, T]`` can be directly used in the rule DSL and match any of the given map's keys and
    push the respective value upon a successful match. The resulting rule type depends on ``T``:

    =================== =========================================
    ``T``               ``R``
    =================== =========================================
    ``Unit``            ``Rule0``
    ``L <: HList``      ``RuleN[L]`` (pushes all values of ``L``)
    ``T`` (otherwise)   ``Rule1[T]`` (pushes only one value)
    =================== =========================================

----

def anyOf(chars: String): Rule0
    This constructs a ``Rule0`` which matches any of the given strings characters.

----

def noneOf(chars: String): Rule0
    This constructs a ``Rule0`` which matches any single character except the ones in the given string and except EOI.

----

def ignoreCase(c: Char): Rule0
    Matches the given single character case insensitively.
    Note: **The given character must be specified in lower-case!** This requirement is currently NOT enforced!

----

def ignoreCase(s: String): Rule0
    Matches the given string of characters case insensitively.
    Note: **The given string must be specified in all lower-case!** This requirement is currently NOT enforced!

----

def ANY: Rule0
    Matches any character except *EOI* (end-of-input).

----

def EOI: Char
    The *EOI* (end-of-input) character, which is a virtual character that the parser "appends" after the last
    character of the actual input.

----

def MATCH: Rule0
    Matches no character (i.e. doesn't cause the parser to make any progress) but succeeds always. It's the "empty"
    rule that is mostly used as a neutral element in rule composition.

----

def MISMATCH[I <: HList, O <: HList]: Rule[I, O]
    A rule that always fails. Fits any rule signature.

----

def MISMATCH0: Rule0
    Same as ``MISMATCH`` but with a clearly defined type. Use it (rather then ``MISMATCH``) if the call site doesn't
    clearly "dictate" a certain rule type and using ``MISMATCH`` therefore gives you a compiler error.


Rule Combinators and Modifiers
------------------------------

Rules can be freely combined/modified with these operations:

----

a ~ b
    Two rules ``a`` and ``b`` can be combined with the ``~`` operator resulting in a rule that only matches if first
    ``a`` matches and then ``b`` matches. The computation of the resulting rule type is somewhat involved.
    Here is an illustration (using an abbreviated HList notation):

    ====================== ==================== =========================
    a                      b                    a ~ b
    ====================== ==================== =========================
    ``Rule[, A]``          ``Rule[, B]``        ``Rule[, A:B]``
    ``Rule[A:B:C, D:E:F]`` ``Rule[F, G:H]``     ``Rule[A:B:C, D:E:G:H]``
    ``Rule[A, B:C]``       ``Rule[D:B:C, E:F]`` ``Rule[D:A, E:F]``
    ``Rule[A, B:C]``       ``Rule[D:C, E:F]``   Illegal if ``D`` != ``B``
    ====================== ==================== =========================

----

a | b
    Two rules ``a`` and ``b`` can be combined with the ``|`` operator to form an "ordered choice" in PEG_ speak.
    The resulting rule tries to match ``a`` and succeeds if this succeeds. Otherwise the parser is reset and ``b``
    is tried. This operator can only be used on compatible rules.

----

&(a)
    Creates a "positive syntactic predicate", i.e. a rule that tests if the underlying rule matches but doesn't cause
    the parser to make any progress (i.e. match any input) itself. Also, all effects that the underlying rule might
    have had on the value stack are cleared out, the resulting rule type is therefore always ``Rule0``,
    independently of the type of the underlying rule.

    Note that ``&`` not itself consuming any input can have surprising implications in repeating constructs,
    see `Non-Termination when using Syntactic Predicates`_ for more details.

----

!a
    Creates a "negative syntactic predicate", i.e. a rule that matches only if the underlying one mismatches and vice
    versa. A syntactic predicate doesn't cause the parser to make any progress (i.e. match any input) and also clears
    out all effects that the underlying rule might have had on the value stack. The resulting rule type is therefore
    always ``Rule0``, independently of the type of the underlying rule.

    Note that ``!`` not itself consuming any input can have surprising implications in repeating constructs,
    see `Non-Termination when using Syntactic Predicates`_ for more details.

----

optional(a)
    Runs its inner rule and succeeds even if the inner rule doesn't. The resulting rule type depends on the type
    of the inner rule:

    =================== =======================
    Type of ``a``       Type of ``optional(a)``
    =================== =======================
    ``Rule0``           ``Rule0``
    ``Rule1[T]``        ``Rule1[Option[T]]``
    ``Rule[I, O <: I]`` ``Rule[I, O]``
    =================== =======================

    The last case is a so-called "reduction rule", which leaves the value stack unchanged on a type level.
    This is an example of a reduction rule wrapped with ``optional``:

    .. code:: Scala

        capture(CharPredicate.Digit) ~ optional(ch('h') ~> ((s: String) => s + "hex"))

    The inner rule of ``optional`` here has type ``Rule[String :: HNil, String :: HNil]``, i.e. it pops one ``String``
    off the stack and pushes another one onto it, which means that the number of elements on the value stack as well as
    their types remain the same, even though the actual values might have changed.

    As a shortcut you can also use ``a.?`` instead of ``optional(a)``.

----

zeroOrMore(a)
    Runs its inner rule until it fails, always succeeds. The resulting rule type depends on the type of the inner rule:

    =================== =======================
    Type of ``a``       Type of ``zeroOrMore(a)``
    =================== =======================
    ``Rule0``           ``Rule0``
    ``Rule1[T]``        ``Rule1[Seq[T]]``
    ``Rule[I, O <: I]`` ``Rule[I, O]``
    =================== =======================

    The last case is a so-called "reduction rule", which leaves the value stack unchanged on a type level.
    This is an example of a reduction rule wrapped with ``zeroOrMore``:

    .. code:: Scala

        (factor :Rule1[Int]) ~ zeroOrMore('*' ~ factor ~> ((a: Int, b) => a * b))

    The inner rule of ``zeroOrMore`` here has type ``Rule[Int :: HNil, Int :: HNil]``, i.e. it pops one ``Int``
    off the stack and pushes another one onto it, which means that the number of elements on the value stack as well as
    their types remain the same, even though the actual values might have changed.

    As a shortcut you can also use ``a.*`` instead of ``zeroOrMore(a)``.

----

oneOrMore(a)
    Runs its inner rule until it fails, succeeds if its inner rule succeeded at least once.
    The resulting rule type depends on the type of the inner rule:

    =================== =======================
    Type of ``a``       Type of ``oneOrMore(a)``
    =================== =======================
    ``Rule0``           ``Rule0``
    ``Rule1[T]``        ``Rule1[Seq[T]]``
    ``Rule[I, O <: I]`` ``Rule[I, O]``
    =================== =======================

    The last case is a so-called "reduction rule", which leaves the value stack unchanged on a type level.
    This is an example of a reduction rule wrapped with ``oneOrMore``:

    .. code:: Scala

        (factor :Rule1[Int]) ~ oneOrMore('*' ~ factor ~> ((a: Int, b) => a * b))

    The inner rule of ``oneOrMore`` here has type ``Rule[Int :: HNil, Int :: HNil]``, i.e. it pops one ``Int``
    off the stack and pushes another one onto it, which means that the number of elements on the value stack as well as
    their types remain the same, even though the actual values might have changed.

    As a shortcut you can also use ``a.+`` instead of ``oneOrMore(a)``.

----

xxx.times(a)
    Repeats a rule a given number of times. ``xxx`` can be either a positive ``Int`` value or a range ``(<x> to <y>)``
    whereby both ``<x>`` and ``<y>`` are positive ``Int`` values.
    The resulting rule type depends on the type of the inner rule:

    =================== =======================
    Type of ``a``       Type of ``xxx.times(a)``
    =================== =======================
    ``Rule0``           ``Rule0``
    ``Rule1[T]``        ``Rule1[Seq[T]]``
    ``Rule[I, O <: I]`` ``Rule[I, O]``
    =================== =======================

    The last case is a so-called "reduction rule", which leaves the value stack unchanged on a type level.
    This is an example of a reduction rule wrapped with ``oneOrMore``:

    .. code:: Scala

        (factor :Rule1[Int]) ~ (1 to 5).times('*' ~ factor ~> ((a: Int, b) => a * b))

    The inner rule here has type ``Rule[Int :: HNil, Int :: HNil]``, i.e. it pops one ``Int`` off the stack and pushes
    another one onto it, which means that the number of elements on the value stack as well as their types remain the
    same, even though the actual values might have changed.

----

a.separatedBy(separator: Rule0)
    You can use ``a.separatedBy(b)`` to create a rule with efficient and automatic support for element separators if
    ``a`` is a rule produced by the ``zeroOrMore``, ``oneOrMore`` or ``xxx.times`` modifier and ``b`` is a ``Rule0``.
    The resulting rule has the same type as ``a`` but expects the individual repetition elements to be separated by
    a successful match of the ``separator`` rule.

    As a shortcut you can also use ``a.*(b)`` or ``(a * b)`` instead of ``zeroOrMore(a).separatedBy(b)``.
    The same shortcut also works for ``+`` (``oneOrMore``).

----

a ~!~ b
    Same as `~` but with "cut" semantics, meaning that the parser will never backtrack across this boundary.
    If the rule being concatenated doesn't match a parse error will be triggered immediately.
    Usually you don't need to use this "cut" operator but in certain cases it can help in simplifying grammar
    construction.


Parser Actions
--------------

The `Basic Character Matching`_  rules and the `Rule Combinators and Modifiers`_ allow you to build *recognizers* for
potentially complex languages, but usually your parser is supposed to do more than simply determine whether a given
input conforms to the defined grammar. In order to run custom logic during parser execution, e.g. for creating custom
objects (like an AST_), you will have to add some "actions" to your rules.

----

push(value)
    ``push(value)`` creates a rule that matches no input (but always succeeds, as a rule) and pushes the given value
    onto the value stack. Its rule type depends on the given value:

    ================= =============================================
    Type of ``value`` Type of ``push(value)``
    ================= =============================================
    ``Unit``          ``Rule0`` (identical to ``run`` in this case)
    ``L <: HList``    ``RuleN[L]`` (pushes all values of ``L``)
    ``T`` (otherwise) ``Rule1[T]`` (pushes only one value)
    ================= =============================================

    Also note that, due to the macro expansion the *parboiled2* rule DSL is based on, the given value expression behaves
    like a call-by-name parameter even though it is not marked as one! This means that the argument expression to
    ``push`` is (re-)evaluated for every rule execution.

----

capture(a)
    Wrapping a rule ``a`` with ``capture`` turns that rule into one that pushes an additional ``String`` instance onto
    the value stack (in addition to all values that ``a`` already pushes itself): the input text matched by ``a``.

    For example ``capture(oneOrMore(CharPredicate.Digit))`` has type ``Rule1[String]`` and pushes one value onto the
    value stack: the string of digit characters matched by ``oneOrMore(CharPredicate.Digit)``.

    Another example: ``capture("foo" ~ push(42))`` has type ``Rule2[Int, String]`` and will match input "foo". After
    successful execution the value stack will have the String ``"foo"`` as its top element and ``42`` underneath.

----

test(condition: Boolean): Rule0
    ``test`` implements "semantic predicates". It creates a rule that matches no input and succeeds only if the given
    condition expression evaluates to true. Note that, due to the macro expansion the *parboiled2* rule DSL is based on,
    the given argument behaves like a call-by-name parameter even though it is not marked as one!
    This means that the argument expression to ``test`` is (re-)evaluated for every rule execution, just as if ``test``
    would have been defined as ``def test(condition: => Boolean): Rule0``.

----

a ~> (...)
    The ``~>`` operator is the "action operator" and as such the most frequently used way to add custom logic to a rule.
    It can be applied to any rule and appends action logic to it. The argument to ``~>`` is always a function, what
    functions are allowed and what the resulting rule type is depends on the type of ``a``.

    The basic idea is that the input of the function is popped of the value stack and the result of the function is
    pushed back onto it. In its basic form the ``~>`` operator therefore transforms the top elements of the value stack
    into some other object(s).

    Let's look at some examples:

    .. code:: Scala

        (foo: Rule1[Int]) ~> (i => i * 2)

    This results in a ``Rule1[Int]`` which multiplies the "output" of rule ``foo`` by 2.

    .. code:: Scala

        (foo: Rule2[Int, String]) ~> ((i, s) => s + i.toString)

    This results in a ``Rule1[String]`` which combines the two "outputs" of rule ``foo`` (an ``Int`` and a ``String``)
    into one single ``String``.

    .. code:: Scala

        (foo: Rule2[Int, String]) ~> (_.toDouble)

    This results in a ``Rule2[Int, Double]``. As you can see the function argument to ``~>`` doesn't always have to
    "take" the complete output of the rule its applied to. It can also take fewer or even more elements. Its parameters
    are simply matched left to right against the top of the value stack (the right-most parameter matching the top-level
    element).

    .. code:: Scala

        (foo: Rule1[String]) ~> ((i :Int, s) => s + i.toString)

    This results in a ``Rule[Int :: HNil, String :: HNil]``, i.e. a rule that pops one ``Int`` value off the stack and
    replaces it with a ``String``. Note that, while the parameter types to the action function can be inferred if they
    can be matched against an "output" of the underlying rule, this is not the case for parameters that don't directly
    correspond to an underlying output. In these cases you need to add an explicit type annotation to the respective
    action function parameter(s).

    If an action function returns ``Unit`` it doesn't push anything on the stack. So this rule

    .. code:: Scala

        (foo: Rule1[String]) ~> (println(_))

    has type ``Rule0``.

    Also, an action function can also be a ``Function0``, i.e. a function without any parameters:

    .. code:: Scala

        (foo: Rule1[String]) ~> (() => 42)

    This rule has type ``Rule2[String, Int]`` and is equivalent to this:

    .. code:: Scala

        (foo: Rule1[String]) ~ push(42)

    An action function can also produce more than one output by returning an ``HList`` instance:

    .. code:: Scala

        (foo: Rule1[String]) ~> (s => s.toInt :: 3.14 :: HNil)

    This has type ``Rule2[Int, Double]``.

    One more very useful feature is special support for case class instance creation:

    .. code:: Scala

        case class Person(name: String, age: Int)

        (foo: Rule2[String, Int]) ~> Person

    This has type ``Rule1[Person]``. The top elements of the value stack are popped off and replaced by an instance
    of the case class if they match in number, order and types to the case class members. This is great for building
    AST_-like structures! Check out the Calculator2__ example to see this form in action.

    Note that there is one quirk: For some reason this notation stops working if you explicitly define a companion
    object for your case class. You'll have to write ``~> (Person(_, _))`` instead.

    __ https://github.com/sirthias/parboiled2/blob/master/examples/src/main/scala/org/parboiled2/examples/Calculator2.scala

    And finally, there is one more very powerful action type: the action function can itself return a rule!
    If an action returns a rule this rule is immediately executed after the action application just as if it
    had been concatenated to the underlying rule with the ``~`` operator. You can therefore do things like

    .. code:: Scala

        (foo: Rule1[Int]) ~> (i => test(i % 2 == 0) ~ push(i))

    which is a ``Rule1[Int]`` that only produces even integers and fails for all others. Or, somewhat unusual
    but still perfectly legal:

    .. code:: Scala

        capture("x") ~> (str(_))

    which is a ``Rule0`` that is identical to ``'x' ~ 'x'``.

----

run(expression)
    ``run`` is the most versatile parser action. It can have several shapes, depending on the type of its argument
    expression. If the argument expression evaluates to

    - a rule (i.e. has type ``R <: Rule[_, _]``) the result type of ``run`` is this rule's type (i.e. ``R``) and the
      produced rule is immediately executed.

    - a function with 1 to 5 parameters these parameters are mapped against the top of the value stack, popped
      and the function executed. Thereby the function behaves just like an action function for the ``~>`` operator,
      i.e. if it produces a ``Unit`` value this result is simply dropped. ``HList`` results are pushed onto the value
      stack (all their elements individually), rule results are immediately executed and other result values are pushed
      onto the value stack as a single element.
      The difference between using ``run`` and attaching an action function with the ``~>`` operator is that in the
      latter case the compiler can usually infer the types of the function parameters (if they map to "output" values
      of the base rule) while with ``run`` you *always* have to explicitly attach type annotation to the function
      parameters.

    - a function with one ``HList`` parameter the behavior is similar to the previous case with the difference that the
      elements of this parameter ``HList`` are mapped against the value stack top. This allows for consumption of an
      arbitrary number of value stack elements (Note: This feature of ``run`` is not yet currently implemented.)

    - any other value the result type of ``run`` is an always succeeding ``Rule0``. Since in this case it doesn't
      interact with the value stack and doesn't match any input all it can do is perform "unchecked" side effects.
      Note that by using ``run`` in this way you are leaving the "safety-net" that the value stack and the rule type
      system gives you! Make sure you understand what you are doing before using these kinds of ``run`` actions!

    Also note that, due to the macro expansion the *parboiled2* rule DSL is based on, the given block behaves like a
    call-by-name parameter even though it is not marked as one! This means that the argument expression to ``run`` is
    (re-)evaluated for every rule execution.

----

runSubParser(f: ParserInput ⇒ Rule[I, O]): Rule[I, O]
    This action allows creation of a sub parser and running of one of its rules as part of the current parsing process.
    The subparser will start parsing at the current input position and the outer parser (the one calling
    ``runSubParser``) will continue where the sub-parser stopped.

----

There are a few more members of the ``Parser`` class that are useful for writing efficient action logic:

def cursor: Int
    The index of the next (yet unmatched) input character.
    Note: Might be equal to ``input.length`` if the cursor is currently behind the last input character!

def cursorChar: Char
    The next (yet unmatched) input character, i.e. the one at the ``cursor`` index.
    Identical to ``if (cursor < input.length) input.charAt(cursor) else EOI`` but more efficient.

def lastChar: Char
    Returns the last character that was matched, i.e. the one at index ``cursor - 1`` and as such is equivalent
    to ``charAt(-1)``. Note that for performance optimization this method does *not* do a range check, i.e. depending on
    the ``ParserInput`` implementation you might get an exception when calling this method before any character was
    matched by the parser.

def charAt(offset: Int): Char
    Returns the character at the input index with the given delta to the cursor and as such is equivalent to
    ``input.charAt(cursor + offset)``. Note that for performance optimization this method does *not* do a range check,
    i.e. depending on the ``ParserInput`` implementation you might get an exception if the computed index is out of
    bounds.

def charAtRC(offset: Int): Char
    Same as ``charAt`` but range-checked. Returns the input character at the index with the given offset from the
    cursor. If this index is out of range the method returns ``EOI``.

You can use these to write efficient character-level logic like this:

.. code:: Scala

    def hexDigit: Rule1[Int] = rule {
      CharPredicate.HexAlpha ~ push(CharUtils.hexValue(lastChar))
    }


Additional Helpers
------------------

Base64Parsing
    For parsing RFC2045_ (Base64) encoded strings *parboiled* provides the ``Base64Parsing`` trait which you can
    mix into your ``Parser`` class. See `its source`_ for more info on what exactly it provides.
    *parboiled* also comes with the ``org.parboiled2.util.Base64`` class which provides an efficient Base64
    encoder/decoder for the standard as well as custom alphabets.

.. _RFC2045: http://tools.ietf.org/html/rfc2045#section-6.8
.. _its source: https://github.com/sirthias/parboiled2/blob/v2.0.0-RC1/parboiled/src/main/scala/org/parboiled2/Base64Parsing.scala

----

DynamicRuleDispatch
    Sometimes an application cannot fully specify at compile-time which of a given set of rules is to be called at
    runtime. For example, a parser for parsing HTTP header values might need to select the right parser rule for a
    header name that is only known once the HTTP request has actually been read from the network.
    To prevent you from having to write a large (and not really efficient) ``match`` against the header name for
    separating out all the possible cases *parboiled* provides the ``DynamicRuleDispatch`` facility.
    Check out `its test`_ for more info on how to use it.

.. _its test: https://github.com/sirthias/parboiled2/blob/v2.0.0-RC1/parboiled/src/test/scala/org/parboiled2/DynamicRuleDispatchSpec.scala

----

StringBuilding
    For certain high-performance use-cases it is sometimes better to construct Strings that the parser is to
    produce/extract from the input in a char-by-char fashion. To support you in doing this *parboiled* provides
    the ``StringBuilding`` trait which you can mix into your ``Parser`` class.
    It provides convenient access to a **single** and **mutable** ``StringBuilder`` instance.
    As such it operates outside of the value stack and therefore without the full "safety net" that parboiled's
    DSL otherwise gives you. If you don't understand what this means you probably shouldn't be using
    the ``StringBuilding`` trait but resort to ``capture`` and ordinary parser actions instead.


Error Reporting
===============

In many applications, especially with grammars that are not too complex, *parboiled* provides good error reports right
out of the box, without any additional requirements on your part.
However, there are cases where you want to have more control over how parse errors are created and/or formatted.
This section gives an overview over how parse error reporting works in *parboiled* and how you can influence it.

The Error Collection Process
----------------------------

As described in the section about `How the Parser matches Input`_ above the parser consumes input by applying
grammar rules and backtracking in the case of mismatches. As such rule mismatches are an integral part of the parsers
operation and do not generally mean that there is something wrong with the input.
Only when the root rule itself mismatches and the parser has no backtracking options remaining does it become clear that
a parse error is present. At that point however, when the root rule mismatches, the information about where exactly
the problematic input was and which of the many rule mismatches that the parser experienced during the run
were the "bad" ones is already lost.

*parboiled* overcomes this problem by simply re-running the failed parser, potentially many times, and "watching" it as
it tries to consume the erroneous input. With every re-run *parboiled* learns a bit more about the position and nature
of the error and when this analysis is complete a ``ParseError`` instance is constructed and handed to the application
as the result of the parsing run, which can then use the error information on its level (e.g. for formatting it and
displaying it to the user).
Note that re-running the parser in the presence of parse errors does result in unsuccessful parsing runs being
potentially much slower than successful ones. However, since in the vast majority of use cases failed runs constitute
only a small minority of all parsing runs and the normal flow of application logic is disrupted anyway, this slow-down
is normally quite acceptable, especially if it results in better error messages. See the section on
`Limiting Error Re-Runs`_ if this is not true for your application.

In principle the error reporting process looks like this:

1. The grammar's root rule is run at maximum speed against the parser input.
   If this succeeds then all is well and the parsing result is immediately dispatched to the user.

2. If the root rule did not match we know that there we have a parsing error.
   The parser is then run again to establish the "principal error location". The principal error location is the
   first character in the input that could not be matched by any rule during the parsing run. In order words, it is
   the maximum value that the parser's ``cursor`` member had during the parsing run.

3. Once the error location is known the parser is run again. This time all rule mismatches against the input character
   at error location are recorded. These rule mismatches are used to determine what input the grammar "expects" at the
   error location but failed to see. For every such "error rule mismatch" the parser collects the "rule trace", i.e.
   the stack of rules that led to it. Currently this is done by throwing a special exception that bubbles up through
   the JVM call stack and records rule stack information on its way up. A consequence of this design is that the parser
   needs to be re-run once per "error rule mismatch".

4. When all error rule traces have been collected all the relevant information about the parse error has been extracted
   and a ``ParseError`` instance can be constructed and dispatched to the user.

Note: The real process contains a few more steps to properly deal with the ``atomic`` and ``quiet`` markers described
below. However, knowledge of these additional steps is not important for understanding the basic approach for how
``ParseError`` instances are constructed.

Formatting Parse Errors
-----------------------

If a parsing runs fails and you receive a ``ParseError`` instance you can call the ``formatError`` method on your
parser instance to get the error rendered into an error message string:

.. code:: Scala

    val errorMsg = parser.formatError(error)

The ``formatError`` message can also take an explicit ``ErrorFormatter`` as a second argument, which allows you to
influence how exactly the error is to be rendered. For example, in order to also render the rule traces you can do:

.. code:: Scala

    val errorMsg = parser.formatError(error, new ErrorFormatter(showTraces = true))

Look at the signature of the ``ErrorFormatter`` constructor for more information on what rendering options exist.

If you want even more control over the error rendering process you can extend the ``ErrorFormatter`` and override
its methods where you see fit.


Tweaking Error Reporting
------------------------

While the error collection process described above yields all information required for a basic "this character
was not matched and these characters were expected instead" information you sometimes want to have more control
over what exactly is reported as "found" and as "expected".

The ``atomic`` Marker
+++++++++++++++++++++

Since PEG parsers are scanner-less (i.e. without an intermediate "TOKEN-stream") they operate directly on the input
buffer's character level. As such, by default, *parboiled* reports all errors on this character level.

For example, if you run the rule ``"foo" | "fob" | "bar"`` against input "foxes" you'll get this error message::

    Invalid input 'x', expected 'o' or 'b' (line 1, column 3):
    foxes
      ^

While this error message is certainly correct, it might not be what you want to show your users, e.g. because ``foo``,
``fob`` and ``bar`` are regarded as "atomic" keywords of your language, that should either be matched completely or not
at all. In this case you can use the ``atomic`` marker to signal this to the parser.
For example, running the rule ``atomic("foo") | atomic("fob") | atomic("bar")`` against input "foxes" yields this error
message::

    Invalid input "fox", expected "foo", "fob" or "bar" (line 1, column 1):
    foxes
    ^

Of course you can use the ``atomic`` marker on any type of rule, not just string rules. It essentially moves the
reported error position forward from the principal error position and lifts the level at which errors are reported
from the character level to a rule level of your choice.

The ``quiet`` Marker
++++++++++++++++++++

Another problem that more frequently occurs with *parboiled*'s default error reporting is that the list of "expected"
things becomes too long. Often the reason for this are rules that deal match input which can appear pretty much anywhere,
like whitespace or comments.

Consider this simple language:

.. code:: Scala

    def Expr    = rule { oneOrMore(Id ~ Keyword ~ Id).separatedBy(',' ~ WS) ~ EOI }
    def Id      = rule { oneOrMore(CharPredicate.Alpha) ~ WS }
    def Keyword = rule { atomic(("has" | "is") ~ WS) }
    def WS      = rule { zeroOrMore(anyOf(" \t \n")) }

When we run the ``Expr`` rule against input "Tim has money, Tom Is poor" we get this error::

    Invalid input 'I', expected [ \t \n] or Keyword (line 1, column 20):
    Tim has money, Tom Is poor
                       ^

Again the list of "expected" things is technically correct but we don't want to bother the user with the information
that whitespace is also allowed at the error location. The ``quiet`` marker let's us suppress a certain rule from the
expected list if there are also non-quiet alternatives:

.. code:: Scala

    def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

With that change the error message becomes::

    Invalid input 'I', expected Keyword (line 1, column 20):
    Tim has money, Tom Is poor
                       ^

which is what we want.


Naming Rules
++++++++++++

*parboiled* uses a somewhat involved logic to determine what exactly to report as "mismatched" and "expected" for a
given parse error. Essentially the process looks like this:

1. Compare all rule trace for the error and drop a potentially existing common prefix. This is done because, if all
   traces share a common prefix, this prefix can be regarded as the "context" of the error which is probably apparent
   to the user and as such doesn't need to be reported.

2. For each trace (suffix), find the first frame that tried to start its match at the reported error position.
   The string representation of this frame (which might be an assigned name) is selected for "expected" reporting.

3. Duplicate "expected" strings are removed.

So, apart from placing ``atomic`` and ``quiet`` markers you can also influence what gets reported as "expected" by
explicitly naming rules. One way to do this is to pick good names for the rule methods as they automatically attach
their name to their rules. The names of ``val`` or ``def`` members that you use to reference ``CharPredicate``
instances also automatically name the respective rule.

If you don't want to split out rules into their own methods you can also use the ``named`` modifier.
With it you can attach an explicit name to any parser rule. For example, if you run the rule ``foo`` from this snippet:

.. code:: Scala

    def foo = rule { "aa" | atomic("aaa").named("threeAs") | 'b' | 'B'.named("bigB") }

against input ``x`` you'll get this error message::

    Invalid input 'x', expected 'a', threeAs, 'b' or bigB (line 1, column 1):
    x
    ^


Manual Error Reporting
++++++++++++++++++++++

If you want to completely bypass *parboiled*'s built-in error reporting logic you can do so by exclusively relying on
the ``fail`` helper, which causes the parser to immediately and fatally terminate the parsing run with a single
one-frame rule trace with a given "expected" message.

For example, the rule ``"foo" | fail("a true FOO")`` will produce this error when run against ``x``::

    Invalid input 'x', expected a true FOO (line 1, column 1):
    x
    ^


Limiting Error Re-Runs
----------------------

Really large grammars, especially ones with bugs as they commonly appear during development, can exhibit a very large
number of rule traces (potentially thousands) and thus cause the parser to take longer than convenient to terminate an
error parsing run.
In order to mitigate this *parboiled* has a configurable limit on the maximum number of rule traces the parser will
collect during a single error run. The default limit is 24, you can change it by overriding the
``errorTraceCollectionLimit`` method of the ``Parser`` class.


Recovering from Parse Errors
----------------------------

Currently *parboiled* only ever parses up to the very first parse error in the input.
While this is all that's required for a large number of use cases there are applications that do require the ability
to somehow recover from parse errors and continue parsing.
Syntax highlighting in an interactive IDE-like environment is one such example.

Future versions of *parboiled* might support parse error recovery.
If your application would benefit from this feature please let us know in `this github ticket`__.

__ https://github.com/sirthias/parboiled2/issues/42


Advanced Techniques
===================

Meta-Rules
----------

Sometimes you might find yourself in a situation where you'd like to DRY up your grammar definition by factoring out
common constructs from several rule definitions in a "meta-rule" that modifies/decorates other rules.
Essentially you'd like to write something like this (*illegal* code!):

.. code:: Scala

    def expression = rule { bracketed(ab) ~ bracketed(cd) }
    def ab = rule { "ab" }
    def cd = rule { "cd" }
    def bracketed(inner: Rule0) = rule { '[' ~ inner ~ ']' }

In this hypothetical example ``bracketed`` is a meta-rule which takes another rule as parameter and calls it from within
its own rule definition.

Unfortunately enabling a syntax such as the one shown above it not directly possible with *parboiled*.
When looking at how the parser generation in *parboiled* actually works the reason becomes clear.
*parboiled* "expands" the rule definition that is passed as argument to the ``rule`` macro into actual Scala code.
The rule methods themselves however remain what they are: instance methods on the parser class.
And since you cannot simply pass a method name as argument to another method the calls ``bracketed(ab)`` and
``bracketed(cd)`` from above don't compile.

However, there is a work-around which might be good enough for your meta-rule needs:

.. code:: Scala

    def expression = rule { bracketed(ab) ~ bracketed(cd) }
    val ab = () ⇒ rule { "ab" }
    val cd = () ⇒ rule { "cd" }
    def bracketed(inner: () ⇒ Rule0) = rule { '[' ~ inner() ~ ']' }

If you model the rules that you want to pass as arguments to other rules as ``Function0`` instances you *can* pass
them around. Assigning those function instances to ``val`` members avoids re-allocation during *every* execution of
the ``expression`` rule which would come with a potentially significant performance cost.


Common Mistakes
===============

Disregarding Order Choice
-------------------------

There is one mistake that new users frequently make when starting out with writing PEG_ grammars: disregarding the
"ordered choice" logic of the ``|`` operator. This operator always tries all alternatives *in the order that they were
defined* and picks the first match.

As a consequence earlier alternatives that are a prefix of later alternatives will always "shadow" the later ones, the
later ones will never be able to match!

For example in this simple rule

.. code:: Scala

    def foo = rule { "foo" | "foobar" }

"foobar" will never match. Reordering the alternatives to either "factor out" all common prefixes or putting the more
specific alternatives first are the canonical solutions.

If your parser is not behaving the way you expect it to watch out for this "wrong ordering" problem, which might be
not that easy to spot in more complicated rule structures.


Non-Termination when using Syntactic Predicates
-----------------------------------------------

The syntactic predicate operators, ``&`` and ``!``, don't themselves consume any input, so directly wrapping them with a
repeating combinator (like ``zeroOrMore`` or ``oneOrMore``) will lead to an infinite loop as the parser continuously
runs the syntactic predicate against the very same input position without making any progress.

If you use syntactic predicates in a loop make sure to actually consume input as well. For example:

.. code:: Scala

    def foo = rule { capture(zeroOrMore( !',' )) }

will never terminate, while

.. code:: Scala

   def foo = rule { capture(zeroOrMore( !',' ~ ANY )) }

will capture all input until it reaches a comma.


Unchecked Mutable State
-----------------------

*parboiled2* parsers work with mutable state as a design choice for achieving good parsing performance. Matching input
and operating on the value stack happen as side-effects to rule execution and mutate the parser state.
However, as long as you confine yourself to the value stack and do not add parser actions that mutate custom parser
members the rule DSL will protect you from making mistakes.

It is important to understand that, in case of rule mismatch, the parser state (cursor and value stack) is reset to
what it was before the rule execution was started. However, if you write rules that have side-effects beyond matching
input and operating on the value stack than these side-effects *cannot* be automatically rolled-back!
This means that you will have to make sure that you action logic "cleans up after itself" in the case of rule mismatches
or is only used in locations where you know that rule execution can never fail.
These techniques are considered advanced and are not recommended for beginners.

The rule DSL is powerful enough to support even very complex parsing logic without the need to resort to custom mutable
state, we consider the addition of mutable members as an optimization that should be well justified.


Handling Whitespace
-------------------

One disadvantage of PEGs over lexer-based parser can be the handling of white space. In a "traditional" parser with a
separate lexer (scanner) phase this lexer can simply skip all white space and only generate tokens for the actual
parser to operate on. This can free the higher-level parser grammar from all white space treatment.

Since PEGs do not have a lexer but directly operate on the raw input they have to deal with white space in the grammar
itself. Language designers with little experience in PEGs can sometime be unsure of how to best handle white space in
their grammar.

The common and highly recommended pattern is to
**match white space always immediately after a terminal (a single character or string) but not in any other place**.
This helps with keeping your grammar rules properly structured and white space "taken care of" without it getting in the
way.

----

In order to reduce boilerplate in your grammar definition parboiled allows for cleanly factoring out whitespace matching
logic into a dedicated rule. By defining a custom implicit conversion from ``String`` to ``Rule0`` you can implicitly
match whitespace after a string terminal:

.. code:: Scala

    class FooParser(val input: ParserInput) extends Parser {
      implicit def wspStr(s: String): Rule0 = rule {
        str(s) ~ zeroOrMore(' ')
      }

      def foo = rule { "foobar" | "foo" } // implicitly matches trailing blanks
      def fooNoWSP = rule { str("foobar") | str("foo") } // doesn't match trailing blanks
    }

In this example all usages of a plain string literals in the parser rules will implicitly match trailing space characters.
In order to *not* apply the implicit whitespace matching in this case simply say ``str("foo")`` instead of just ``"foo"``.


Parsing the whole Input
-----------------------

If you don't explicitly match ``EOI`` (the special end-of-input pseudo-character) in your grammar's root rule
the parser will not produce an error if, at the end of a parsing run, there is still unmatched input left.
This means that if the root rule matches only a prefix of the whole input the parser will report a successful parsing
run, which might not be what you want.

As an example, consider this very basic parser:

.. code:: Scala

    class MyParser(val input: ParserInput) extends Parser {
      def InputLine = rule { "foo" | "bar }
    }
    
    new MyParser("foo").InputLine.run()  // Success
    new MyParser("foot").InputLine.run()  // also Success!!

In the second run of the parser, instead of failing with a ``ParseError`` as you might expect, it successfully parses
the matching input ``foo`` and ignores the rest of the input.

If this is not what you want you need to explicitly match ``EOI``, for example as follows:

.. code:: Scala

    def InputLine = rule { ("foo" | "bar) ~ EOI }


Grammar Debugging
=================

TODO

(e.g., use ``parse.formatError(error, showTraces = true)``)


Access to Parser Results
========================

In order to run the top-level parser rule against a given input you create a new instance of your parser class and
call ``run()`` on it, e.g:

.. code:: Scala

    val parser = new MyParser(input)
    val result = parser.rootRule.run()

By default the type of ``result`` in this snippet will be a ``Try[T]`` whereby ``T`` depends on the type
of ``rootRule``:

================================= ==========================
Type of ``rootRule``              Type of ``rootRule.run()``
================================= ==========================
``Rule0``                         ``Try[Unit]``
``Rule1[T]``                      ``Try[T]``
``RuleN[L <: HList]`` (otherwise) ``Try[L]``
================================= ==========================

The contents of the value stack at the end of the ``rootRule`` execution constitute the result of the parsing run.
Note that ``run()`` is not available on rules that are not of type ``RuleN[L <: HList]``.

If the parser is not able to match the input successfully it creates an instance of class ``ParseError`` , which is
defined like this

.. code:: Scala

    case class ParseError(position: Position, charCount: Int, traces: Seq[RuleTrace]) extends RuntimeException

In such cases the ``Try`` is completed with a ``scala.util.Failure`` holding the ``ParseError``.
If other exceptions occur during the parsing run (e.g. because some parser action failed) these will also end up as
a ``Try`` failure.

*parboiled2* has quite powerful error reporting facilities, which should help you (and your users) to easily understand
why a particular input does not conform to the defined grammar and how this can be fixed.
The ``formatError`` method available on the ``Parser`` class is of great utility here, as it can "pretty print"
a parse error instance, to display something like this (excerpt from the ErrorReportingSpec_)::

    Invalid input 'x', expected 'f', Digit, hex or UpperAlpha (line 1, column 4):
    abcx
       ^

    4 rules mismatched at error location:
      targetRule / | / "fgh" / 'f'
      targetRule / | / Digit
      targetRule / | / hex
      targetRule / | / UpperAlpha


Alternative DeliverySchemes
---------------------------

Apart from delivering your parser results as a ``Try[T]`` *parboiled2* allows you to select another one of the
pre-defined ``Parser.DeliveryScheme`` alternatives, or even define your own. They differ in how they wrap the three
possible outcomes of a parsing run:

- parsing completed successfully, deliver a result of type ``T``
- parsing failed with a ``ParseError``
- parsing failed due to another exception

This table compares the built-in ``Parser.DeliveryScheme`` alternatives (the first one being the default):

=================================== ========================== ======= ========== ================
Import                              Type of ``rootRule.run()`` Success ParseError Other Exceptions
=================================== ========================== ======= ========== ================
import Parser.DeliveryScheme.Try    Try[T]                     Success Failure    Failure
import Parser.DeliveryScheme.Either Either[ParseError, T]      Right   Left       thrown
import Parser.DeliveryScheme.Throw  T                          T       thrown     thrown
=================================== ========================== ======= ========== ================

.. _ErrorReportingSpec: https://github.com/sirthias/parboiled2/blob/master/parboiled/src/test/scala/org/parboiled2/ErrorReportingSpec.scala


Running the Examples
====================

Follow these steps to run the example parsers defined here__ on your own machine:

1. Clone the *parboiled2* repository::

    git clone git://github.com/sirthias/parboiled2.git

2. Change into the base directory::

    cd parboiled2

3. Run SBT::

    sbt "project examples" run

__ https://github.com/sirthias/parboiled2/tree/master/examples/src/main/scala/org/parboiled2/examples


Alternatives
============

parboiled2 vs. parboiled 1.x
----------------------------

TODO

(about one order of magnitude faster, more powerful DSL, improved error reporting, fewer dependencies (more lightweight),
but Scala 2.10.3+ only, no error recovery (yet) and no Java version (ever))


parboiled2 vs. Scala Parser Combinators
---------------------------------------

TODO

(several hundred times (!) faster, better error reporting, more concise and elegant DSL, similarly powerful in terms of
language class capabilities, but Scala 2.10.3+ only, 2 added dependencies (parboiled2 + shapeless))

parboiled2 vs. Regular Expressions
----------------------------------

TODO

(much easier to read and maintain, more powerful (e.g. regexes do not support recursive structures), faster,
but Scala 2.10.3+ only, 2 added dependencies (parboiled2 + shapeless))


Roadmap
=======

TODO


Contributing
============

TODO


Support
=======

In most cases the `parboiled2 mailing list`__ is probably the best place for your needs with regard to
support, feedback and general discussion.

**Note:** Your first post after signup is going to be moderated (for spam protection), but we'll immediately
give you full posting privileges if your message doesn't unmask you as a spammer.

__ https://groups.google.com/forum/#!forum/parboiled-user

You can also use the gitter.im chat channel for parboiled2:

.. image:: https://badges.gitter.im/Join%20Chat.svg
   :alt: Join the chat at https://gitter.im/sirthias/parboiled2
   :target: https://gitter.im/sirthias/parboiled2?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge


References
==========

TODO


Credits
=======

Much of *parboiled2* was developed by `Alexander Myltsev`__ during `GSoc 2013`__, a big thank you for his great work!

Also, without the `Macro Paradise`__ made available by `Eugene Burmako`__ *parboiled2* would probably still not be ready
and its codebase would look a lot more messy.


__ https://github.com/alexander-myltsev
__ http://www.google-melange.com/gsoc/homepage/google/gsoc2013
__ http://docs.scala-lang.org/overviews/macros/paradise.html
__ https://github.com/xeno-by


License
=======

*parboiled2* is released under the `Apache License 2.0`__

__ http://en.wikipedia.org/wiki/Apache_license

.. |--| unicode:: U+2013
