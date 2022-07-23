//
// Improving the parser library
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/
//

let log = Js.log
let log2 = Js.log2

// updated parser for Ch3 with labels
let strToChar = String.get(_, 0)
let charToStr = String.make(1, _)
let isDigit = c => (c >= '1' && '9' >= c) || c == '0'
let isWhitespace = c => c == ' '

let charListToString = chars =>
  chars->Belt.List.map(Char.escaped)->Belt.List.reduce("", Js.String2.concat)

let compose = (f, g, x) => g(f(x))

type parserLabel = string
type parserError = string

type parseResult<'a> =
  | Success('a)
  | Failure(parserLabel, parserError)

// A Parser structure has a parsing function & label
type parser<'a> = Parser({parseFn: string => parseResult<('a, string)>, label: parserLabel})

let printResult = result => {
  switch result {
  | Success(value, _input) => value->log
  | Failure(label, error) => `Error parsing ${label}\n${error}`->log
  }
}

/// get the label from a parser
let getLabel = (Parser({label})) => label

// infix version of setLabel
// let ( <?> ) = setLabel
let setLabel = (Parser({parseFn, label}), newLabel) => {
  let newInnerFn = input => {
    let result = parseFn(input)
    switch result {
    | Success(s) => Success(s)
    | Failure(oldLabel, err) => Failure(newLabel, err)
    }
  }
  Parser({parseFn: newInnerFn, label: newLabel})
}

// Match an input token if the predicate is satisfied
let satisfy = (predicate, label) => {
  let innerFn = input => {
    module String = Js.String2
    if input->String.length == 0 {
      Failure(label, "No more input")
    } else {
      let first = input->String.charAt(0)->strToChar
      if first->predicate {
        let remainingInput = input->String.sliceToEnd(~from=1)
        Success(first, remainingInput)
      } else {
        let err = `Unexpected ${first->charToStr}`
        Failure(label, err)
      }
    }
  }
  Parser({parseFn: innerFn, label: label})
}

// Parse a single character
let pchar = charToMatch => {
  let predicate = ch => ch == charToMatch
  let lablel = charToMatch->charToStr
  satisfy(predicate, lablel)
}

// Run a parser with some input
let run = (Parser({parseFn}), input) => {
  // call inner function with input
  parseFn(input)
}

// "bindP" takes a parser-producing function f, and a parser p
// and passes the output of p into f, to create a new parser

// Infix version of bindP
// let ( >>= ) p f = bindP f p
//
let bindP = (f, p) => {
  let label = "unknown"
  let innerFn = input => {
    let result1 = p->run(input)
    switch result1 {
    | Failure(label, err) => Failure(label, err)
    | Success(value1, remainingInput) => {
        let p2 = f(value1)
        p2->run(remainingInput)
      }
    }
  }
  Parser({parseFn: innerFn, label: label})
}

// Lift a value to a Parser
//
let returnP = x => {
  let label = ""
  let innerFn = input => {
    Success(x, input)
  }
  Parser({parseFn: innerFn, label: label})
}

// apply a function to the value inside a parser
//
// infix version of mapP
// let ( <!> ) = mapP
//
// "piping" version of mapP
// let ( |>> ) x f = mapP f x
//
let mapP = f => bindP(compose(f, returnP), _)

// apply a wrapped function to a wrapped value
//
// infix version of apply
// let ( <*> ) = applyP
//
let applyP = (fP, xP) => {
  fP->bindP(f => {
    xP->bindP(x => {
      returnP(f(x))
    }, _)
  }, _)
}

// lift a two parameter function to Parser World
let lift2 = (f, xP, yP) => {
  returnP(f)->applyP(xP)->applyP(yP)
}

// Combine two parsers as "A andThen B"
//
// Infix version of andThen
// let ( .>>. ) = andThen
let andThen = (p1, p2) => {
  let label = `${p1->getLabel} andThen ${p2->getLabel}`
  p1->bindP(p1Result => {
    p2->bindP(p2Result => {
      returnP((p1Result, p2Result))
    }, _)
  }, _)->setLabel(label)
}

// Combine two parsers as "A orElse B"
//
// Infix version of orElse
// let ( <|> ) = orElse
let orElse = (parser1, parser2) => {
  let label = `${getLabel(parser1)} orElse ${getLabel(parser2)}`
  let innerFn = input => {
    let result1 = run(parser1, input)

    switch result1 {
    | Success(result) => result1

    | Failure(_, err) => {
        let result2 = run(parser2, input)

        result2
      }
    }
  }
  Parser({parseFn: innerFn, label: label})
}

// Choose any of a list of parsers
//
let choice = listOfParsers => {
  let label = "choice"
  listOfParsers->Belt.List.reduce(
    Parser({parseFn: string => Failure("Initial parser", "Initial parser"), label: label}),
    orElse,
  )
}

// Choose any of a list of characters
//
let anyOf = listOfChars => {
  let label = `any of ${listOfChars->charListToString}`

  listOfChars->Belt.List.map(pchar)->choice->setLabel(label)
}

// Convert a list of Parsers into a Parser of a list
//
let rec sequence = parserList => {
  let cons = (head, tail) => list{head, ...tail}
  let consP = lift2(cons)

  switch parserList {
  | list{} => returnP(list{})
  | list{head, ...tail} => consP(head, sequence(tail))
  }
}

// (helper) match zero or more occurrences of the specified parser
//
let rec parseZeroOrMore = (parser, input) => {
  let firstResult = run(parser, input)

  switch firstResult {
  | Failure(_, _) => (list{}, input)
  | Success(firstValue, inputAfterFirstParse) => {
      let (subsequentValues, remainingInput) = parseZeroOrMore(parser, inputAfterFirstParse)
      let values = list{firstValue, ...subsequentValues}
      (values, remainingInput)
    }
  }
}

// matches zero or more occurrences of the specified parser
//
let many = parser => {
  let label = `many ${getLabel(parser)}`
  let innerFn = input => {
    Success(parseZeroOrMore(parser, input))
  }
  Parser({parseFn: innerFn, label: label})
}

// matches one or more occurrences of the specified parser
//
let many1 = p => {
  p->bindP(head => {
    many(p)->bindP(tail => {
      returnP(list{head, ...tail})
    }, _)
  }, _)
}

// Parses an optional occurrence of p and returns an option value.
//
let opt = p => {
  let some = mapP(x => Some(x), p)
  let none = returnP(None)
  some->orElse(none)
}

// Keep only the result of the left side parser
// keepLeft (.>>)
//
let keepLeft = (p1, p2) => {
  p1->andThen(p2)->mapP(((a, b)) => a, _)
}

// Keep only the result of the right side parser
// keepRight (>>.)
//
let keepRight = (p1, p2) => {
  p1->andThen(p2)->mapP(((a, b)) => b, _)
}

// Keep only the result of the middle parser
//
let between = (p1, p2, p3) => {
  p1->keepRight(p2)->keepLeft(p3)
}

// Parses one or more occurrences of p separated by sep
//
let sepBy1 = (p, sep) => {
  let sepThenP = sep->keepRight(p)
  p->andThen(many(sepThenP))->mapP(((p, pList)) => list{p, ...pList}, _)
}

// Parses zero or more occurrences of p separated by sep
//
let sepBy = (p, sep) => {
  sepBy1(p, sep)->orElse(returnP(list{}))
}

"-- Improving the parser library"->log

""->log
"1. Labelling a Parser"->log

"-- Updating the code"->log

"-- Updating the label"->log

// Update the label in the parser
//

let digit = {
  "0123456789"->Js.String2.split("")->Belt.Array.map(strToChar)->Belt.List.fromArray->anyOf
}

let parseDigit_WithLabel = digit->setLabel("digit")

run(parseDigit_WithLabel, "|ABC")->printResult

"-- 2. Replacing pchar with satisfy"->log
let digitChar = {
  let predicate = isDigit
  let label = "digit"
  satisfy(predicate, label)
}

let whitespaceChar = {
  let predicate = isWhitespace
  let label = "whitespace"
  satisfy(predicate, label)
}

"-- 3. Adding position and context to error messages"->log
