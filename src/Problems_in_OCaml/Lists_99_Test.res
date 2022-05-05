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

test("16. drop", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->drop(3)
  let expected = list{"a", "b", "d", "e", "g", "h", "j"}
  listEqual(result, expected)
})

test("17. split", () => {
  let (r1, r2) = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->split(3)
  let e1 = list{"a", "b", "c"}
  let e2 = list{"d", "e", "f", "g", "h", "i", "j"}
  listEqual(r1, e1)
  listEqual(r2, e2)
})

test("18. slice", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->slice(2, 6)
  let expected = list{"c", "d", "e", "f", "g"}
  listEqual(result, expected)
})

test("19. rotate", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h"}->rotate(3)
  let expected = list{"d", "e", "f", "g", "h", "a", "b", "c"}
  listEqual(result, expected)
})

test("20. remove_at", () => {
  let result = list{"a", "b", "c", "d"}->remove_at(1)
  let expected = list{"a", "c", "d"}
  listEqual(result, expected)
})

test("21. insert_at", () => {
  let result = list{"a", "b", "c", "d"}->insert_at(1, "alfa")->Belt.List.toArray

  let expected = list{"a", "alfa", "b", "c", "d"}->Belt.List.toArray
  stringArrayEqual(result, expected)
})

test("22. range", () => {
  let result = range(4, 9)->Belt.List.toArray
  let expected = list{4, 5, 6, 7, 8, 9}->Belt.List.toArray
  intArrayEqual(result, expected)
})

test("23. range", () => {
  let result = range_tail_recur(3, 11)->Belt.List.toArray
  let expected = range(3, 11)->Belt.List.toArray
  intArrayEqual(result, expected)
})

test("24. rand_select", () => {
  let result =
    rand_select(list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}, 3)->Belt.List.toArray
  let expected = list{"d", "i", "e"}->Belt.List.toArray
  intArrayEqual(result, expected)
})
