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
  | Handled(str) => str->Utils.log
  | Unhandled(n) => n->string_of_int->Utils.log
  }

let fizbuzz = n =>
  n
  -> handle(~divisor=15, ~label="FizzBuzz")
  -> ifUnhandledDo(handle(~divisor=3, ~label="Fizz"))
  -> ifUnhandledDo(handle(~divisor=5, ~label="Buzz"))
  -> lastStep

12->fizbuzz
10->fizbuzz
15->fizbuzz
1->fizbuzz
