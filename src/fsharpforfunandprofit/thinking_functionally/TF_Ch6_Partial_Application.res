//
// ref: https://fsharpforfunandprofit.com/posts/partial-application/
//
@@uncurried
@@uncurried.swap

open Belt
let log = Js.log

let add42 = x => x + 42

1->add42->log
3->add42->log

list{1, 2, 3}->Belt.List.map(add42)->log

let twoIsLessThan = x => x > 2
twoIsLessThan(1)->log
twoIsLessThan(3)->log

list{1, 2, 3}->Belt.List.map(twoIsLessThan)->log

// create an adder that supports a pluggable logging function
let adderWithPluggableLogger = (. logger, x, y) => {
  logger("x", x)
  logger("y", y)
  let result = x + y
  logger("x+y", result)
  result
}

// create a logging function that writes to the console
let consoleLogger = (argName, argValue) => `${argName}=${argValue->Int.toString}`->Js.log

//create an adder with the console logger partially applied
let addWithConsoleLogger = adderWithPluggableLogger(consoleLogger)
addWithConsoleLogger(1, 2)->ignore
addWithConsoleLogger(42, 99)->ignore

// create a logging function that uses red text
let redLogger = (argName, argValue) => {
  let message = `${argName}=${argValue->Int.toString}`
  Js.log("[red]" ++ message)
}

//create an adder with the popup logger partially applied
let addWithRedLogger = adderWithPluggableLogger(redLogger)
addWithRedLogger(1, 2)->ignore
addWithRedLogger(42, 99)->ignore
