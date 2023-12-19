open RescriptCore

// Monad Type
type numberWithLogs = {
  result: int,
  logs: array<string>,
}

let square = (x: int) => {
  let result = x * x
  {
    result,
    logs: [`Squared ${x->Int.toString} to get ${result->Int.toString}`],
  }
}

let addOne = (x: int) => {
  let result = x + 1
  {
    result,
    logs: [`Added 1 to ${x->Int.toString} to get to ${result->Int.toString}`],
  }
}

// Monad constructor (wrap function)
let wrapWithLog = (x: int) => {
  result: x,
  logs: [],
}

// Monad `bind`, `flatMap`, `>>=`
let runWithLogs = (input: numberWithLogs, transform: int => numberWithLogs) => {
  let resultWithLogs = transform(input.result)
  {
    result: resultWithLogs.result,
    logs: input.logs->Belt.Array.concat(resultWithLogs.logs),
  }
}

let a = wrapWithLog(5)
let b = runWithLogs(a, addOne)
runWithLogs(b, square)->Js.log

//
// ReScript Option Monad
//

open Belt
// type
type str = option<string>

// wrap function
let peter = Some("Peter")
let noone = None

// run function is Belt.Option.flatMap
let isABoy = x => x->Js.String2.concat(" is a boy")->Some

peter->Option.flatMap(isABoy)->Js.log
noone->Option.flatMap(isABoy)->Js.log
