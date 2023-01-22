//
// https://github.com/reazen/relude-parse
//
let log = Js.Console.log
let log2 = Js.Console.log2

module P = ReludeParse.Parser
open P.Infix

// E.g. 127.0.0.1
//
// There are many different ways to compose a parser to parse values like this. Below are just some
// examples to show different techniques.

type t = IPv4(int, int, int, int)
let make = (a, b, c, d) => IPv4(a, b, c, d)

// Using a tuple and mapTuple4
// parse a short (up to 255) and the dot separators
let parseWithTuple =
  (
    P.anyPositiveShort->\"<*"(P.str(".")),
    P.anyPositiveShort->\"<*"(P.str(".")),
    P.anyPositiveShort->\"<*"(P.str(".")),
    P.anyPositiveShort,
  )->P.mapTuple4(make, _)

parseWithTuple->P.runParser("127.0.0.1", _)->log2("with tuple\n", _)

// Using nested flatMaps and a final map at the end.
// These are nested because we have to collect each value as we go, and it has to
// be in scope at the end when we want to construct our final value.
// Note: language support for sequences of monadic binds (e.g. do notation or the
// upcoming let+/let* bindings in OCaml, this becomes a beautiful, flat expression,
// almost like imperative code, but with pure FP data structures and functions!
let parseWithNestedFlatMaps =
  P.anyPositiveShort->\">>="(a =>
    P.str(".")->\">>="(_ =>
      P.anyPositiveShort->\">>="(
        b =>
          P.str(".")->\">>="(
            _ =>
              P.anyPositiveShort->\">>="(
                c => P.str(".")->\">>="(_ => P.anyPositiveShort->\"<#>"(d => make(a, b, c, d))),
              ),
          ),
      )
    )
  )

parseWithNestedFlatMaps->P.runParser("127.0.0.1", _)->log2("with nested flatMaps\n", _)

// With a monadic flow, another technique is to map each successive result into
// an ever-expanding tuple, and pass the values along that way. This allows you
// to have a more flat structure, at the
// expense of wrapping and unwrapping tuples at each step.

// Using <$> and <*> (with <* and *> helpers)
// Our make function is (int, int, int, int) => IPv4
// The first map <$> creates a `Parser.t((int, int, int) => IPv4)`
// and each successive <*> fills another slot in our function,
// until we finally collect the 4 args.
// The `<* str(".")` reads a ".", but throw it away.
let parseWithMonadicFlow =
  make
  ->\"<$>"(P.anyPositiveShort) // collect a positive short
  ->\"<*"(P.str(".")) // read and ignore .
  ->\"<*>"(P.anyPositiveShort) // collect a positive short
  ->\"<*"(P.str(".")) // read and ignore .
  ->\"<*>"(P.anyPositiveShort) // collect a positive short
  ->\"<*"(P.str(".")) // read and ignore .
  ->\"<*>"(P.anyPositiveShort) // collect a positive short

parseWithMonadicFlow->P.runParser("127.0.0.1", _)->log2("with monadic flow\n", _)

// Using sepBy
// Note that sepBy produces a list of the values produced by the value parser,
// so we have to manually validate that we got the correct number in our list.
// This is done using `>>=`, so we can fail the parse with the `fail` function,
// which produces a failing parser.  If we get the 4 values we need, we use pure
// to create a parser that produces our desired IPv4 value.
let parseWithSepBy =
  P.anyPositiveShort
  ->P.sepBy(P.str("."), _)
  ->\">>="(// sepBy gives us a list, so we have to pick the parts out
  shorts =>
    switch shorts {
    | list{a, b, c, d} =>
      // Use pure here because we need to wrap the result in a parser to satisfy
      // the type signature of the >>= function
      P.pure(make(a, b, c, d))
    | _ => P.fail("Expected exactly 4 shorts separated by .")
    // fail produces a `Parser.t(_)` that will always fail
    }
  )

parseWithSepBy->P.runParser("127.0.0.1", _)->log2("with sepBy\n", _)
