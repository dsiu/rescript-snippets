open Jest2
open FizzBuzz

test("FizzBuzz 12", () => {
  expect(12->fizzbuzz)->toEqual("Fizz")
})

test("FizzBuzz 10", () => {
  expect(10->fizzbuzz)->toEqual("Buzz")
})

test("FizzBuzz 15", () => {
  expect(15->fizzbuzz)->toEqual("FizzBuzz")
})

test("FizzBuzz 1", () => {
  expect(1->fizzbuzz)->toEqual("1")
})
