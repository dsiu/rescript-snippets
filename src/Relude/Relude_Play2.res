let log = Console.log
open Relude.Function
let formula = (x, y) => {x + 2 * y}
formula(3, 5)->log // 13
formula(5, 3)->log // 11

Relude.Function.flip(formula, 5, 3)->log // 13

let square = x => {x * x}
let double = x => {2 * x}
Relude.Function.flipCompose(square, double, 3)->log // 18;
Relude.Function.flipCompose(double, square, 3)->log // 36;

let square = x => {x * x}
let double = x => {2 * x}
let addFive = x => {x + 5}

let formula = square->(@res.partial andThen(double))->(andThen(addFive, ...))
formula(3)->log // 23
let showResult = n => {
  (x: float) => {
    "input " ++ string_of_int(n) ++ " yields " ++ Js.Float.toString(x)
  }
}

let cube = x => {float_of_int(x * x * x)}

apply(showResult, cube)(5)->log // input 5 yields 125
