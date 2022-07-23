//
// Building a useful set of parser combinators
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
//
let log = Js.log
let log2 = Js.log2

// from Ch1
let strToChar = String.get(_, 0)
let charToStr = String.make(1, _)

type parseResult<'a> =
  | Success('a)
  | Failure(string)

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

let run = (parser, input) => {
  let Parser(innerFn) = parser
  innerFn(input)
}

// let ( .>>. ) = andThen
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

// let ( <|> ) = orElse
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

let choice = listOfParsers => {
  listOfParsers->Belt.List.reduce(Parser(string => Failure("Initial parser")), orElse)
}

let anyOf = listOfChars => {
  listOfChars->Belt.List.map(pchar)->choice
}

"Building a useful set of parser combinators"->log

"-- 1. Transforming the contents of a parser with map"->log

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

""->log
"-- 2. Lifting functions to the world of Parsers"->log

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

""->log
"-- 3. Turning a list of Parsers into a single Parser"->log

let rec sequence = parserList => {
  let cons = (head, tail) => list{head, ...tail}
  let consP = lift2(cons)

  switch parserList {
  | list{} => returnP(list{})
  | list{head, ...tail} => consP(head, sequence(tail))
  }
}

let parsers = list{pchar('A'), pchar('B'), pchar('C')}
let combined = sequence(parsers)

run(combined, "ABCD")->log2("combined ABCD", _)

""->log
"-- Implementing the pstring parser"->log

let charListToStr = charList => {
  charList->Belt.List.map(charToStr)->Belt.List.toArray->Js.Array2.joinWith("")
}

let pstring = str => {
  str
  ->Js.String2.split("")
  ->Belt.Array.map(strToChar)
  ->Belt.Array.map(pchar)
  ->Belt.List.fromArray
  ->sequence
  ->mapP(charListToStr, _)
}

let parseABC = pstring("ABC")

run(parseABC, "ABCDE")->log2("parseABC ABCDE", _)
run(parseABC, "A|CDE")->log2("parseABC A|CDE", _)
run(parseABC, "AB|DE")->log2("parseABC AB|DE", _)

""->log
"-- 4. Matching a parser multiple times"->log

let rec parseZeroOrMore = (parser, input) => {
  let firstResult = run(parser, input)

  switch firstResult {
  | Failure(err) => (list{}, input)
  | Success(firstValue, inputAfterFirstParse) => {
      let (subsequentValues, remainingInput) = parseZeroOrMore(parser, inputAfterFirstParse)
      let values = list{firstValue, ...subsequentValues}
      (values, remainingInput)
    }
  }
}

let many = parser => {
  let innerFn = input => {
    Success(parseZeroOrMore(parser, input))
  }
  Parser(innerFn)
}

let manyA = many(pchar('A'))
run(manyA, "ABCD")->log2("manyA ABCD", _)
run(manyA, "AACD")->log2("manyA AACD", _)
run(manyA, "AAAD")->log2("manyA AAAD", _)
run(manyA, "|BCD")->log2("manyA |BCD", _)

let manyAB = many(pstring("AB"))
run(manyAB, "ABCD")->log2("manyAB ABCD", _)
run(manyAB, "ABABCD")->log2("manyAB ABABCD", _)
run(manyAB, "ZCD")->log2("manyAB ZCD", _)
run(manyAB, "AZCD")->log2("manyAB AZCD", _)

let whitespaceChar = anyOf(list{' ', '\t', '\n'})
let whitespace = many(whitespaceChar)

run(whitespace, "ABC")->log2("whitespace ABC", _)
run(whitespace, " ABC")->log2("whitespace  ABC", _)
run(whitespace, " \tABC")->log2("whitespace \tABC", _)

""->log
"-- Defining many1"->log

let many1 = parser => {
  let innerFn = input => {
    let firstResult = run(parser, input)

    switch firstResult {
    | Failure(err) => Failure(err)

    | Success(firstValue, inputAfterFirstParse) => {
        let (subsequentValues, remainingInput) = parseZeroOrMore(parser, inputAfterFirstParse)
        let values = list{firstValue, ...subsequentValues}

        Success(values, remainingInput)
      }
    }
  }
  Parser(innerFn)
}

let digit = {
  "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
}

let digits = many1(digit)

run(digits, "1ABC")->log2("digits 1ABC", _)
run(digits, "12BC")->log2("digits 12BC", _)
run(digits, "123C")->log2("digits 123C", _)
run(digits, "1234")->log2("digits 1234", _)
run(digits, "ABC")->log2("digits ABC", _)

""->log
"-- Parsing an integer"->log

let pint = {
  let resultToInt = digitList => {
    digitList
    ->Belt.List.map(charToStr)
    ->Belt.List.toArray
    ->Js.Array2.joinWith("")
    ->Belt.Int.fromString
    ->Belt.Option.getExn
  }

  let digit = {
    "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
  }

  let digits = many1(digit)

  digits->mapP(resultToInt, _)
}

run(pint, "1ABC")->log2("pint 1ABC", _)
run(pint, "12BC")->log2("pint 12BC", _)
run(pint, "123C")->log2("pint 123C", _)
run(pint, "1234")->log2("pint 1234", _)
run(pint, "ABC")->log2("pint ABC", _)

"-- 5. Matching a parser zero or one time"->log

let opt = p => {
  let some = mapP(x => Some(x), p)
  let none = returnP(None)
  some->orElse(none)
}

let digitThenSemicolon = {
  digit->andThen(opt(pchar(';')))
}

run(digitThenSemicolon, "1;")->log2("digitThenSemicolon 1;", _)
run(digitThenSemicolon, "1")->log2("digitThenSemicolon 1;", _)

let pint = {
  let resultToInt = ((sign, charList)) => {
    let i =
      charList
      ->Belt.List.map(charToStr)
      ->Belt.List.toArray
      ->Js.Array2.joinWith("")
      ->Belt.Int.fromString
      ->Belt.Option.getExn
    switch sign {
    | Some(ch) => -i
    | None => i
    }
  }

  let digit = {
    "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
  }

  let digits = many1(digit)

  opt(pchar('-'))->andThen(digits)->mapP(resultToInt, _)
}

run(pint, "123C")->log2("pint 123C", _)
run(pint, "-123C")->log2("pint 123C", _)

"-- 6. Throwing results away"->log
