//
// https://github.com/reazen/relude-parse
//
@@uncurried
@@uncurried.swap

let log = Js.Console.log
let log2 = Js.Console.log2

module P = ReludeParse.Parser
open P.Infix // Get all the infix operators in scope
let _ = P.anyDigit->\"<*"(P.eof)->P.runParser("", _)->(log2("infix\n", _))

// anyDigit will attempt to consume a single character, and succeeds if that character
// is a digit 0-9

// Pipe operator
P.anyDigit->P.runParser("1", _)->(log2("Pipe Op\n", _))

// Or normal function application
P.runParser("1", P.anyDigit)->(log2("Normal Fn\n", _))

//
// Mapping a function over a parser
//

// Warning: int_of_string is unsafe (can throw) - this is just an example
P.anyDigit->P.map(int_of_string, _)->P.runParser("1", _)->(log2("map\n", _))
// Belt.Result.Ok(1)

// <$> map operator version.  <$> is traditionally "function on the left"
int_of_string->\"<$>"(P.anyDigit)->P.runParser("1", _)->(log2("map <$>\n", _))
// Belt.Result.Ok(1)

// <#> flipped map operator version - <#> is function on the right hand side,
// which might be more readable for some.
// When you see `<#>` just think `.map(...)` from JavaScript
P.anyDigit->\"<$$>"(int_of_string)->P.runParser("1", _)->(log2("<#>\n", _))

//
// Combining parsers (via Applicative)
//
// Combine two parsers into a tuple of the results (assuming all succeed)
P.tuple2(P.anyDigit, P.anyDigit)->P.runParser("12", _)->(log2("combine 2 parsers\n", _))
// Belt.Result.Ok(("1", "2"))

// <^> operator (operator version of tuple2)
P.anyDigit->\"<^>"(P.anyDigit)->P.runParser("12", _)->(log2("<^> tuple2\n", _))
// Belt.Result.Ok(("1", "2"))

// Combine more parsers using tuple3 up to tuple5
P.tuple3(P.anyDigit, P.anyDigit, P.anyDigit)->P.runParser("123", _)->(log2("tuple3\n", _))
// Belt.Result.Ok(("1", "2", "3"))

// Combine parse results using a function via map2 through map5
P.map2((a, b) => a + b, P.anyDigitAsInt, P.anyDigitAsInt)
->P.runParser("12", _)
->(log2("combine with map\n", _))
// Belt.Result.Ok(3)

// Combine results from a tuple of parsers using mapTuple2 through mapTuple5
(P.anyDigitAsInt, P.anyDigitAsInt)
->P.mapTuple2((a, b) => a + b, _)
->P.runParser("12", _)
->(log2("mapTuple2\n", _))
// Belt.Result.Ok(3)

// Use the *> operator to run two parsers, and only keep the result from the right side
// This is useful if you don't care what the left side parser produces (e.g. whitespace)
// but you need to consume that input.
// `ws` consumes all the whitespace it encounters and throws it away
P.ws->\"*>"(P.anyDigit)->P.runParser("   3", _)->(log2("*>\n", _))
// Belt.Result.Ok("3")

// Use the <* operator to run two parsers, and only keep the result from the left side
// This is useful if you don't care what the right side parser produces (e.g. whitespace)
// but you want to consume that input.
// It's common to use this operator with `eof` to make sure you've hit the end of the input
// (but you don't care about the value produced by `eof`).
// E.g. use both *> and <* to trim whitespace surrounding a value
P.ws->\"*>"(P.anyDigit)->\"<*"(P.ws)->\"<*"(P.eof)->P.runParser("   3  ", _)->(log2("*> <*\n", _))
// Belt.Result.Ok("3")

//
// ADVANCED: Incrementally collect parse results using a function and chained <$> map and <*> apply
// operators.
//
let add3 = a => b => c => a + b + c
add3
->\"<$>"(P.anyDigitAsInt)
->\"<*>"(P.anyDigitAsInt)
->\"<*>"(P.anyDigitAsInt)
->P.runParser("123", _)
->(log2("<$> and <*>\n", _))
// Belt.Result.Ok(6)

//
// Sequencing parsers (via Monads)
//

// Lift a pure value into a parser.
// As you can see the parser just produces the given value regardless of the string.
P.pure(3)->P.runParser("abcdef", _)->(log2("life a value\n", _))
// Belt.Result.Ok(3)

// Sequence parse operations using flatMap.
//
// In this example we read a single digit as an int, then use that value
// to read a series of letters, and expect to consume the whole input.
// This is sequencing because we use the result of one parser to determine
// the next parser to run.
P.anyDigitAsInt
->P.flatMap(count => P.anyAlpha->P.times(count, _)->\"<*"(P.eof), _)
->P.map(chars => Relude.List.String.join(chars), _)
->P.runParser("3abc", _)
->(log2("sequencing\n", _))
// Belt.Result.Ok("abc")

// Sequence using >>= (flatMap/bind) and <#> (map) operators.
//
// If you are coming from JS -
// Don't be afraid of the operators - when you see >>= read ".flatMap(...)"
// and when you see "<#>" read ".map(...)".  Eventually these will become
// second nature.
P.anyDigitAsInt
->\">>="(count => P.times(count, P.anyAlpha)->\"<*"(P.eof))
->\"<$$>"(Relude.List.String.join)
->P.runParser("3abc", _)
->(log2("sequencing with <#>\n", _))
// Belt.Result.Ok("abc")

//
// Add validation and error handling in a parse chain
//
P.anyDigitAsInt
->\">>="(count =>
  if count >= 5 {
    // P.fail is a parser that always fails with the given message
    // just like P.pure always succeeds with the given value.
    // Using >>= and fail is a common way to inject validations and raise errors.
    P.fail("The count cannot be >= 5")
  } else {
    // Now that we have a valid count, carry on
    P.times(count, P.anyAlpha)->\"<*"(P.eof)
  }
)
->\"<$$>"(Relude.List.String.join)
->P.runParser("9abc", _)
->(log2("validation\n", _))
// Belt.Result.Error(ParseError("The count cannot be >= 5"))

P.anyDigitAsInt
->P.filter(a => a > 5, _)
->\"<?>"("Expected an int greater than 5")
->(log2("customize error message\n", _))

//
// Trying multiple parsers (via Alt)
//
// ReludeParse.Parser.t('a) is also an alt functor, which means you can try one parser, and if it
// fails, try another, as many times as you want.
//
// The <|> operator is used for this - think of the <|> operator as an orElse function.
P.anyDigit->\"<|>"(P.anyAlpha)->P.runParser("9", _)->(log2("alt <|>\n", _))
// Belt.Result.Ok("9")
P.anyDigit->\"<|>"(P.anyAlpha)->P.runParser("a", _)->(log2("alt <|>\n", _))
// Belt.Result.Ok("a")
P.anyDigit->\"<|>"(P.anyAlpha)->P.runParser("!", _)->(log2("alt <|>\n", _))
// Belt.Result.Error(...)

// <|> can be chained as many times as you want - it attempts each parser left-to-right.
P.str("a")->\"<|>"(P.str("b")->\"<|>"(P.str("c")->\"<|>"(P.str("d"))))->(log2("alt <|> many\n", _))

// If none of the parsers succeed, it will return the error of the last parser, so a common
// technique is to use <?> to add a custom error message at the end
P.str("a")
->\"<|>"(P.str("b")->\"<|>"(P.str("c")))
->\"<?>"("Expected a, b, or c")
->(log2("alt with custom error\n", _))

// Sometimes when using <|> with more complex parsers, the first parser might consume some input
// before failing, which might mess up the next parser in the <|> chain. Use the tries function to
// force a parser to back-track all the way to it's original position if it fails.

// Without tries, this fails, because the first parser consumes the 9, then fails,
// but the next parser wants to consume a digit then a letter.  Using tries makes the
// parser fully back-track on failure if it had consumed any input.
P.tries(P.anyDigit->\"*>"(P.anyDigit)) // parse a digit, throw it away, then parse another digit
->\"<|>"(P.anyDigit->\"*>"(P.anyAlpha)) // parse a digit,throw it away, then parse a letter
->P.runParser("9a", _)
->(log2("tries to back-track\n", _))
// Belt.Result.Ok("a")

//
// Customizing the error message
//
// Use the <?> operator to put a custom error message on a parser. This is useful if you are
// composing a more complex parser from smaller parsers, and want a more meaningful error message if
// the parser fails.
P.many1(P.anyDigit)
->\"<?>"("Expected one or more digits")
->P.runParser("abc", _)
->(log2("custom error\n", _))
// Belt.Result.Error(ParseError("Expected one or more digits"))

//
// Checking that all input is consumed
//
// To make sure that all the input in the string has been consumed, use the eof (end-of-file)
// parser. It's common to use <* eof to parse the end of input, because <* will just keep what's on
// the left side of eof.
P.anyDigit->\"<*"(P.eof)->P.runParser("3", _)->(log2("check input consumed\n", _)) // Succeeds for "3" but fails for "3"
P.anyDigit->\"<*"(P.eof)->P.runParser("3 ", _)->(log2("check input consumed\n", _)) // Succeeds for "3" but fails for "3 "

//
// Debugging
//
// Use tap to inspect the result of a successful parse, and the parse position.
// Use the tapLog function to inject some basic logging anywhere in a parser composition.
let tl1 = P.anyDigit->\"*>"(P.anyDigit->\"*>"(P.anyAlpha))->P.tapLog
tl1->P.runParser("12a", _)->(log2("Debugging - tagLog\n", _)) // etc.
tl1->P.runParser("b34", _)->(log2("Debugging - tagLog\n", _)) // etc.
