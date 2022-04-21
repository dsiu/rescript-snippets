open Test
open FizzBuzz
open Test_Utils

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
