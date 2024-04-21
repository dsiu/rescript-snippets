///
// https://cs3110.github.io/textbook/chapters/ds/monads.html
//

let log = Js.log
let log2 = Js.log2

//let \"+" = Pervasives.\"+"
//let \"-" = Pervasives.\"-"
//let \"*" = Pervasives.\"*"
//let \"/" = Pervasives.\"/"

let plus_opt = (x: option<int>, y: option<int>): option<int> => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => Some(a + b)
  }
}

let minus_opt = (x: option<int>, y: option<int>): option<int> => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => Some(a - b)
  }
}

let mult_opt = (x: option<int>, y: option<int>): option<int> => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => Some(a - b)
  }
}

let div_opt = (x: option<int>, y: option<int>): option<int> => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => b === 0 ? None : Some(a / b)
  }
}

//let x = Some(1) + Some(4) / Some(2)

//
// lets refactor some of the code
//
let propagate_none_try: ((int, int) => int, option<int>, option<int>) => option<int> = (
  op,
  x,
  y,
) => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => Some(op(a, b))
  }
}
//let \"+" = propagate_none_try(\"+")
//let \"-" = propagate_none_try(\"-")
//let \"*" = propagate_none_try(\"*")

// can't do with div because it doesn't check for div by 0
// let do a better propagate_none
let propagate_none: ((int, int) => option<int>, option<int>, option<int>) => option<int> = (
  op,
  x,
  y,
) => {
  switch (x, y) {
  | (None, _) | (_, None) => None
  | (Some(a), Some(b)) => op(a, b)
  }
}

let wrap_output: ((int, int) => int, int, int) => option<int> = (op, x, y) => Some(op(x, y))

let \"+" = propagate_none(wrap_output(\"+", ...), ...)
let \"-" = propagate_none(wrap_output(\"-", ...), ...)
let \"*" = propagate_none(wrap_output(\"*", ...), ...)
let \"/" = propagate_none((x, y) => y === 0 ? None : wrap_output(\"/", x, y), ...)

//let \"+" = Pervasives.\"+"
//let \"-" = Pervasives.\"-"
//let \"*" = Pervasives.\"*"
//let \"/" = Pervasives.\"/"
