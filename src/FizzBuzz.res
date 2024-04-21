@@uncurried
@@uncurried.swap

type fizzBuzzResult =
  | Unhandled(int)
  | Handled(string)

let handle = (n, ~divisor, ~label) =>
  switch n->mod(divisor) {
  | 0 => Handled(label)
  | _ => Unhandled(n)
  }

let ifUnhandledDo = (result, f) =>
  switch result {
  | Handled(str) => Handled(str)
  | Unhandled(n) => n->f
  }

let lastStep = x =>
  switch x {
  | Handled(str) => str
  | Unhandled(n) => n->string_of_int
  }

let fizzbuzz = n =>
  n
  ->handle(~divisor=15, ~label="FizzBuzz")
  ->ifUnhandledDo(handle(~divisor=3, ~label="Fizz", ...))
  ->ifUnhandledDo(handle(~divisor=5, ~label="Buzz", ...))
  ->lastStep
