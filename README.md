parboiled2: a Macro-Based PEG Parser Generator for Scala
==========

Grammar-based parsing of text data is a ubiquitous problem in real-world applications. One popular technique for implementing parsers is parser combinators. However, even though it's comparatively easy to build a parser with combinators the end-result is logic that essentially "interprets" the grammar rules against the input, which is rather slow. Hand-writing a parser can yield a much faster implementation but is tedious and error-prone.

It is possible to implement a parser defined via an embedded DSL that is translated into runnable code by the host language compiler at compile time. The macro support introduced with Scala 2.10 provides the required and sufficient infrastructure for such an endeavor.

The goal of this project is to use Scala 2.10 macros to implement a compile-time generator for highly efficient PEG parsers. Rule definition is based on the parboiled DSL embedded in Scala with a few extensions (such as shapeless' HLists for combining flexibility with strong type-safety).


Compile it: `sbt compile`

Test it: `sbt test`


Roadmap
-------

June 2013: First working version with all PEG features implemented

July 2013: Tweaks and error reporting:

August 2013: Extensions (unrolling of left-recursions, continuation-parsing)


Notes
-----

The project is a candidate to `GSoC'13 Scala Projects` (http://www.scala-lang.org/gsoc2013)

Google Groups thread related to the project: https://groups.google.com/forum/?fromgroups=#!topic/scala-language/sXC-f88Adiw