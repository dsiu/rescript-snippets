open Jest
open Expect
open! Expect.Operators
open FizzBuzz

describe("FizzBuzz", () => {
  test("FizzBuzz 12", () => {
    12->fizzbuzz -> expect |> toBe("Fizz")
  })

  test("FizzBuzz 10", () => {
    10->fizzbuzz  -> expect |> toBe("Buzz")
//    15->fizzbuzz -> expect |> toBe("Fizz")
//    1->fizzbuzz -> expect |> toBe("Fizz")
  })

  test("FizzBuzz 15", () => {
    15->fizzbuzz -> expect |> toBe("FizzBuzz")
//    1->fizzbuzz -> expect |> toBe("Fizz")
  })

  test("FizzBuzz 1", () => {
    1->fizzbuzz -> expect |> toBe("1")
  })
})
