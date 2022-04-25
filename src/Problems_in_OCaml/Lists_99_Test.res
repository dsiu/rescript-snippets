open Test
open Test_Utils

open Lists_99

@@warning("-26")
test("1. last", () => {
  optionEqual(list{"a", "b", "c", "d"}->last, Some("d"))
  optionEqual(list{}->last, None)
})

test("2. last_two", () => {
  optionListEqual(list{"a", "b", "c", "d"}->last_two, Some(list{"c", "d"}))
  optionListEqual(list{"a"}->last_two, None)
})

test("3. at", () => {
  optionEqual(list{"a", "b", "c", "d", "e"}->at(3), Some("c"))
  optionEqual(list{"a"}->at(3), None)
})

test("4. length", () => {
  intEqual(list{"a", "b", "c", "d", "e"}->length, 5)
  intEqual(list{"a"}->length, 1)
  intEqual(list{}->length, 0)
})

test("5. rev", () => {
  listEqual(list{"a", "b", "c", "d", "e"}->rev, list{"e", "d", "c", "b", "a"})
  listEqual(list{}->rev, list{})
})

test("6. is_palindrome", () => {
  boolEqual(list{"x", "a", "m", "a", "x"}->is_palindrome, true)
  boolEqual(list{"a", "b", "c"}->is_palindrome, false)
  boolEqual(list{"a"}->is_palindrome, true)
  boolEqual(list{}->is_palindrome, true)
  //  listEqual(list{}->rev, list{})
})

test("7. flatten", () => {
  listEqual(
    list{One("a"), Many(list{One("b"), Many(list{One("c"), One("d")}), One("e")})}->flatten,
    list{"a", "b", "c", "d", "e"},
  )
})

test("8. compress", () => {
  listEqual(
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}->compress,
    list{"a", "b", "c", "a", "d", "e"},
  )
  listEqual(list{"a", "a"}->compress, list{"a"})
  listEqual(list{"a"}->compress, list{"a"})
})

test("9. pack", () => {
  let result =
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "d", "e", "e", "e", "e"}
    ->pack
    ->Belt.List.map(Belt.List.toArray)
    ->Belt.List.toArray

  let expected = [
    ['a', 'a', 'a', 'a'],
    ['b'],
    ['c', 'c'],
    ['a', 'a'],
    ['d', 'd'],
    ['e', 'e', 'e', 'e'],
  ]
})

test("10. encode", () => {
  let result =
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    ->encode
    ->Belt.List.toArray

  let expected = [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
})

test("11. encode'", () => {
  let result =
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    ->encode_11
    ->Belt.List.toArray
    ->Js.log2("11. encode result", _)

  let expected =
    [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")]->Js.log2(
      "11. encode expected",
      _,
    )
})

test("12. decode", () => {
  let result =
    list{Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")}
    ->decode
    ->Belt.List.toArray
    ->Js.log2("12. decode result", _)

  let expected =
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    ->Belt.List.toArray
    ->Js.log2("12. decode expected", _)
})

test("13. encode", () => {
  let result =
    list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    ->encode_13
    ->Belt.List.toArray
    ->Js.log2("13. encode result", _)

  let expected =
    [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")]->Js.log2(
      "13. encode expected",
      _,
    )
})

test("14. duplicate", () => {
  let result = list{"a", "b", "c", "c", "d"}->duplicate
  let expected = list{"a", "a", "b", "b", "c", "c", "c", "c", "d", "d"}
  listEqual(result, expected)
})

test("15. replicate", () => {
  let result = replicate(list{"a", "b", "c"}, 3)
  let expected = list{"a", "a", "a", "b", "b", "b", "c", "c", "c"}
  listEqual(result, expected)
})
