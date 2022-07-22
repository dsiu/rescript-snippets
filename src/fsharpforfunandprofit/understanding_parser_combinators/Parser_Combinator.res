//
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
//
let log = Js.log
let log2 = Js.log2

let strToChar = String.get(_, 0)
let charToStr = String.make(1, _)

let parseA = str => {
  module String = Js.String2
  if str->String.length == 0 {
    (false, "")
  } else if str->String.charAt(0)->strToChar == 'A' {
    let remaining = str->String.sliceToEnd(~from=1)
    (true, remaining)
  } else {
    (false, str)
  }
}

let inputABC = "ABC"
parseA(inputABC)->log2("parseA", _)

let inputZBC = "ZBC"
parseA(inputZBC)->log2("parseA", _)

"-- Parsing a specified character"->log

let pchar = (charToMatch, str) => {
  module String = Js.String2
  if str->String.length == 0 {
    let msg = "No more input"
    (msg, "")
  } else {
    let first = str->String.charAt(0)->strToChar
    if first == charToMatch {
      let remaining = str->String.sliceToEnd(~from=1)
      let msg = `Found ${charToMatch->charToStr}`
      (msg, remaining)
    } else {
      let msg = `Expecting '${charToMatch->charToStr}'. Got '${first->charToStr}'`
      (msg, str)
    }
  }
}

pchar('A', inputABC)->log2("pchar", _)
pchar('A', inputZBC)->log2("pchar", _)
pchar('Z', inputZBC)->log2("pchar", _)

"-- Returning a Success/Failure"->log

type parseResult<'a> =
  | Success('a)
  | Failure(string)

let pchar = (charToMatch, str) => {
  module String = Js.String2
  if str->String.length == 0 {
    Failure("No more input")
  } else {
    let first = str->String.charAt(0)->strToChar
    if first == charToMatch {
      let remaining = str->String.sliceToEnd(~from=1)
      Success(charToMatch, remaining)
    } else {
      let msg = `Expecting '${charToMatch->charToStr}'. Got '${first->charToStr}'`
      Failure(msg)
    }
  }
}

pchar('A', inputABC)->log2("pchar", _)
pchar('A', inputZBC)->log2("pchar", _)

"-- Rewriting with an inner function"->log

let pchar = charToMatch => {
  let innerFn = str => {
    module String = Js.String2
    if str->String.length == 0 {
      Failure("No more input")
    } else {
      let first = str->String.charAt(0)->strToChar
      if first == charToMatch {
        let remaining = str->String.sliceToEnd(~from=1)
        Success(charToMatch, remaining)
      } else {
        let msg = `Expecting '${charToMatch->charToStr}'. Got '${first->charToStr}'`
        Failure(msg)
      }
    }
  }
  innerFn
}

"-- The benefits of the curried implementation"->log

let parseA = pchar('A')
parseA(inputABC)->log2("parseA", _)
parseA(inputZBC)->log2("parseA", _)

"Encapsulating the parsing function in a type"->log

type parser<'a> = Parser(string => parseResult<('a, string)>)

let pchar = charToMatch => {
  let innerFn = str => {
    module String = Js.String2
    if str->String.length == 0 {
      Failure("No more input")
    } else {
      let first = str->String.charAt(0)->strToChar
      if first == charToMatch {
        let remaining = str->String.sliceToEnd(~from=1)
        Success(charToMatch, remaining)
      } else {
        let msg = `Expecting '${charToMatch->charToStr}'. Got '${first->charToStr}'`
        Failure(msg)
      }
    }
  }
  Parser(innerFn)
}

"-- Testing the wrapped function"->log

let run = (parser, input) => {
  let Parser(innerFn) = parser
  innerFn(input)
}
let parseA = pchar('A')
run(parseA, inputABC)->log2("parseA", _)
run(parseA, inputZBC)->log2("parseA", _)

"-- Combining two parsers in sequence"->log

let andThen = (parser1, parser2) => {
  let innerFn = input => {
    let result1 = run(parser1, input)

    switch result1 {
    | Failure(err) => Failure(err)

    | Success(value1, remaining1) => {
        let result2 = run(parser2, remaining1)

        switch result2 {
        | Failure(err) => Failure(err)

        | Success(value2, remaining2) => {
            let newValue = (value1, value2)
            Success(newValue, remaining2)
          }
        }
      }
    }
  }
  Parser(innerFn)
}

// let ( .>>. ) = andThen

"-- Testing andThen"->log

let parseA = pchar('A')
let parseB = pchar('B')
let parseAThenB = parseA->andThen(parseB)

run(parseAThenB, "ABC")->log2("parseAThenB ABC", _)
run(parseAThenB, "ZBC")->log2("parseAThenB ZBC", _)
run(parseAThenB, "AZC")->log2("parseAThenB AZC", _)

"-- Choosing between two parsers"->log

let orElse = (parser1, parser2) => {
  let innerFn = input => {
    let result1 = run(parser1, input)

    switch result1 {
    | Success(result) => result1

    | Failure(err) => {
        let result2 = run(parser2, input)

        result2
      }
    }
  }
  Parser(innerFn)
}

// let ( <|> ) = orElse

"-- Testing orElse"->log

let parseA = pchar('A')
let parseB = pchar('B')
let parseAOrElseB = parseA->orElse(parseB)

run(parseAOrElseB, "AZZ")->log2("parseAOrElseB AZZ", _)
run(parseAOrElseB, "BZZ")->log2("parseAOrElseB BZZ", _)
run(parseAOrElseB, "CZZ")->log2("parseAOrElseB CZZ", _)

"-- Combining andThen and orElse"->log
let parseA = pchar('A')
let parseB = pchar('B')
let parseC = pchar('C')
let bOrElseC = parseB->orElse(parseC)
let aAndThenBorC = parseA->andThen(bOrElseC)

run(aAndThenBorC, "ABZ")->log2("aAndThenBorC ABZ", _)
run(aAndThenBorC, "ACZ")->log2("aAndThenBorC ACZ", _)
run(aAndThenBorC, "QBZ")->log2("aAndThenBorC QBZ", _)
run(aAndThenBorC, "AQZ")->log2("aAndThenBorC AQZ", _)

"-- Choosing from a list of parsers"->log

let choice = listOfParsers => {
  listOfParsers->Belt.List.reduce(Parser(string => Failure("Initial parser")), orElse)
}

let anyOf = listOfChars => {
  listOfChars->Belt.List.map(pchar)->choice
}

let parseLowercase = {
  "abcdefghijklmnopqrstuvwxyz"
  ->Js.String2.split("")
  ->Belt.Array.map(strToChar)
  ->Belt.List.fromArray
  ->anyOf
}

let parseDigit = {
  "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
}

run(parseLowercase, "aBC")->log2("parseLowercase aBC", _)
run(parseLowercase, "ABC")->log2("parseLowercase ABC", _)

run(parseDigit, "1ABC")->log2("parseDigit 1ABC", _)
run(parseDigit, "9ABC")->log2("parseDigit 9ABC", _)
run(parseDigit, "|ABC")->log2("parseDigit |ABC", _)

//
// Building a useful set of parser combinators
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
//
"----------"->log
"Building a useful set of parser combinators"->log

"1. Transforming the contents of a parser with map"->log

let parseDigit = {
  "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
}

let parseThreeDigits = parseDigit->andThen(parseDigit)->andThen(parseDigit)

run(parseThreeDigits, "123A")->log2("parseThreeDigits 123A", _)

let mapP = (f, parser) => {
  let innerFn = input => {
    let result = run(parser, input)

    switch result {
    | Success(value, remaining) => {
        let newValue = f(value)
        Success(newValue, remaining)
      }
    | Failure(err) => Failure(err)
    }
  }
  Parser(innerFn)
}

// let ( <!> ) = mapP
// let ( |>> ) x f = mapP f x

let parseThreeDigitsAsStr = {
  let tupleParser = parseThreeDigits
  let transformTuple = (((c1, c2), c3)) => c1->charToStr ++ c2->charToStr ++ c3->charToStr
  mapP(transformTuple, tupleParser)
}

run(parseThreeDigitsAsStr, "123A")->log2("parseThreeDigitsAsStr 123A", _)

let parseThreeDigitsAsStr = {
  parseDigit
  ->andThen(parseDigit)
  ->andThen(parseDigit)
  ->mapP((((c1, c2), c3)) => {c1->charToStr ++ c2->charToStr ++ c3->charToStr}, _)
}

run(parseThreeDigitsAsStr, "123A")->log2("parseThreeDigitsAsStr 123A", _)

let parseThreeDigitsAsInt = mapP(
  s => s->Belt.Int.fromString->Belt.Option.getExn,
  parseThreeDigitsAsStr,
)

run(parseThreeDigitsAsInt, "123A")->log2("parseThreeDigitsAsInt 123A", _)

"2. Lifting functions to the world of Parsers"->log

let returnP = x => {
  let innerFn = input => {
    Success(x, input)
  }
  Parser(innerFn)
}

let applyP = (fP, xP) => {
  let fxP = andThen(fP, xP)
  fxP->mapP(((f, x)) => f(x), _)
}

// let ( <*> ) = applyP

let lift2 = (f, xP, yP) => {
  returnP(f)->applyP(xP)->applyP(yP)
}

let addP = lift2(\"+")

let startsWith = (str: string, prefix: string) => {
  str->Js.String2.startsWith(prefix)
}

let startWithP = lift2(startsWith)

"3. Turning a list of Parsers into a single Parser"->log

//let rec sequence = parserList => {
//
//}
