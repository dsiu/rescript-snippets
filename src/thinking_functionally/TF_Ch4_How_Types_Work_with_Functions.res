//
// ref: https://fsharpforfunandprofit.com/posts/how-types-work-with-functions/
//

// Function types as parameters
let evalWith5ThenAdd2 = fn => fn(5) + 2

let add1 = x => x + 1

evalWith5ThenAdd2(add1)->Js.log

let times3 = x => x * 3
evalWith5ThenAdd2(times3)->Js.log

// Functions as output
let add = (x, y) => x + y

let add1 = add(1)
let add2 = add(2)

5->add1->Js.log
5->add2->Js.log

let evalWith5AsInt = (fn: int => int) => fn(5)
let evalWith5AsFloat = (fn: int => float) => fn(5)
let evalWith5AsString = (fn): string => fn ++ "5"

// The “unit” type
let whatIsThis = ()
let printInt = x => `x is ${x->Belt.Int.toString}`->Js.log

// Parameterless functions
let printHello = "hello world"->Js.log
printHello

let printHelloFn = () => "hello world"->Js.log
printHelloFn->Js.log
printHelloFn()->Js.log

// Generic types
type toString<'a, 'b> =
  | Ts_Int('a)
  | Ts_Float('b)

let toString = x => {
  switch x {
  | Ts_Int(x) => x->Belt.Int.toString
  | Ts_Float(x) => x->Belt.Float.toString
  }
}

let onAStick = x => x->toString ++ " on a stick"

onAStick(Ts_Int(1))->Js.log
onAStick(Ts_Float(2.0))->Js.log

let a = ("hello", 1)

// Functions with more than two parameters
let multiParamFn = (p1: int, p2: bool, p3: string, p4: float) => {p1->ignore}

let intermediateFn1 = 42->multiParamFn
let intermediateFn2 = false->intermediateFn1
let intermediateFn3 = "hello"->intermediateFn2
let finalResult = 3.141->intermediateFn3
