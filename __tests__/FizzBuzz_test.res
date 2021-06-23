open Test
open FizzBuzz

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

test("FizzBuzz 12", () => {
  stringEqual(12->fizzbuzz, "Fizz")
})

test("FizzBuzz 10", () => {
  stringEqual(10->fizzbuzz, "Buzz")
})

test("FizzBuzz 15", () => {
  stringEqual(15->fizzbuzz, "FizzBuzz")
})

test("FizzBuzz 1", () => {
  stringEqual(1->fizzbuzz, "1")
})
