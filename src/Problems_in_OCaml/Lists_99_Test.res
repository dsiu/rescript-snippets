open Jest
open Expect

open Lists_99

@@warning("-26")
describe("List_99", () => {
  let test1 = list{(list{"a", "b", "c", "d"}->last, Some("d")), (list{}->last, None)}
  testAll("1. last", test1, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test2 = list{
    (list{"a", "b", "c", "d"}->last_two, Some(list{"c", "d"})),
    (list{"a"}->last_two, (None)),
  }

  testAll("2. last_two", test2, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test3 = list{list{"a", "b", "c", "d", "e"}->at(3), Some("c"), list{"a"}->at(3), None}

  testAll("3. at", test3, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })


  let test4 = list{
  (list{"a", "b", "c", "d", "e"}->length), (5),
  (list{"a"}->length), (1),
  (list{}->length), (0),
  }

  testAll("4. length", test4, ((result, expected)) => {
  expect(result)->toEqual(expected)

})



test("5. rev", () => {
  expect(list{"a", "b", "c", "d", "e"}->rev)->toEqual(list{"e", "d", "c", "b", "a"})
  expect(list{}->rev)->toEqual(list{})
})

test("6. is_palindrome", () => {
  expect(list{"x", "a", "m", "a", "x"}->is_palindrome)->toEqual(true)
  expect(list{"a", "b", "c"}->is_palindrome)->toEqual(false)
  expect(list{"a"}->is_palindrome)->toEqual(true)
  expect(list{}->is_palindrome)->toEqual(true)
  //  expect(list{}->rev)->toEqual(list{})
})

test("7. flatten", () => {
  expect(
    Belt.List.eq(
      list{One("a"), Many(list{One("b"), Many(list{One("c"), One("d")}), One("e")})}->flatten,
      list{"a", "b", "c", "d", "e"},
    ),
  )->toEqual(true)
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
  expect(result)->toEqual(expected)
})

test("15. replicate", () => {
  let result = replicate(list{"a", "b", "c"}, 3)
  let expected = list{"a", "a", "a", "b", "b", "b", "c", "c", "c"}
  expect(result)->toEqual(expected)
})

test("16. drop", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->drop(3)
  let expected = list{"a", "b", "d", "e", "g", "h", "j"}
  expect(result)->toEqual(expected)
})

test("17. split", () => {
  let (r1, r2) = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->split(3)
  let e1 = list{"a", "b", "c"}
  let e2 = list{"d", "e", "f", "g", "h", "i", "j"}
  expect(r1)->toEqual(e1)
  expect(r2)->toEqual(e2)
})

test("18. slice", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->slice(2, 6)
  let expected = list{"c", "d", "e", "f", "g"}
  expect(result)->toEqual(expected)
})

test("19. rotate", () => {
  let result = list{"a", "b", "c", "d", "e", "f", "g", "h"}->rotate(3)
  let expected = list{"d", "e", "f", "g", "h", "a", "b", "c"}
  expect(result)->toEqual(expected)
})

test("20. remove_at", () => {
  let result = list{"a", "b", "c", "d"}->remove_at(1)
  let expected = list{"a", "c", "d"}
  expect(result)->toEqual(expected)
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
