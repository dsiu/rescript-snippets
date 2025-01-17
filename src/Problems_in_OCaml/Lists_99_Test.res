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
    (list{"a"}->last_two, None),
  }

  testAll("2. last_two", test2, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test3 = list{(list{"a", "b", "c", "d", "e"}->at(3), Some("c")), (list{"a"}->at(3), None)}

  testAll("3. at", test3, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test4 = list{
    (list{"a", "b", "c", "d", "e"}->length, 5),
    (list{"a"}->length, 1),
    (list{}->length, 0),
  }

  testAll("4. length", test4, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test5 = list{
    (list{"a", "b", "c", "d", "e"}->rev, list{"e", "d", "c", "b", "a"}),
    (list{}->rev, list{}),
  }

  testAll("5. rev", test5, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test6 = list{
    (list{"x", "a", "m", "a", "x"}->is_palindrome, true),
    (list{"a", "b", "c"}->is_palindrome, false),
    (list{"a"}->is_palindrome, true),
    (list{}->is_palindrome, true),
    //(list{}->rev, list{}),
  }

  testAll("6. is_palindrome", test6, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test7 = list{
    (
      list{One("a"), Many(list{One("b"), Many(list{One("c"), One("d")}), One("e")})}->flatten,
      list{"a", "b", "c", "d", "e"},
    ),
  }

  testAll("7. flatten", test7, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test8 = list{
    (
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}->compress,
      list{"a", "b", "c", "a", "d", "e"},
    ),
    (list{"a", "a"}->compress, list{"a"}),
    (list{"a"}->compress, list{"a"}),
  }

  testAll("8. compress", test8, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test9 = list{
    (
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "d", "e", "e", "e", "e"}
      ->pack
      ->Belt.List.map(Belt.List.toArray)
      ->Belt.List.toArray,
      [["a", "a", "a", "a"], ["b"], ["c", "c"], ["a", "a"], ["d", "d"], ["e", "e", "e", "e"]],
    ),
  }

  testAll("9. pack", test9, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test10 = list{
    (
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
      ->encode
      ->Belt.List.toArray,
      [(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")],
    ),
  }

  testAll("10. encode", test10, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test11 = list{
    (
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
      ->encode_11
      ->Belt.List.toArray,
      [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")],
    ),
  }

  testAll("11. encode", test11, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test12 = list{
    (
      list{Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")}
      ->decode
      ->Belt.List.toArray,
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}->Belt.List.toArray,
    ),
  }

  let testAll12 = testAll("12. decode", test12, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test13 = list{
    (
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
      ->encode_13
      ->Belt.List.toArray,
      [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")],
    ),
  }

  testAll("13. encode", test13, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test14 = list{
    (
      list{"a", "b", "c", "c", "d"}->duplicate,
      list{"a", "a", "b", "b", "c", "c", "c", "c", "d", "d"},
    ),
  }

  testAll("14. duplicate", test14, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test15 = list{
    (replicate(list{"a", "b", "c"}, 3), list{"a", "a", "a", "b", "b", "b", "c", "c", "c"}),
  }

  testAll("15. replicate", test15, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test16 = list{
    (
      list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->drop(3),
      list{"a", "b", "d", "e", "g", "h", "j"},
    ),
  }

  testAll("16. drop", test16, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test17 = list{
    (
      list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->split(3),
      (list{"a", "b", "c"}, list{"d", "e", "f", "g", "h", "i", "j"}),
    ),
  }

  testAll("17. split", test17, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test18 = list{
    (
      list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}->slice(2, 6),
      list{"c", "d", "e", "f", "g"},
    ),
  }

  testAll("18. slice", test18, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test19 = list{
    (
      list{"a", "b", "c", "d", "e", "f", "g", "h"}->rotate(3),
      list{"d", "e", "f", "g", "h", "a", "b", "c"},
    ),
  }

  testAll("19. rotate", test19, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test20 = list{(list{"a", "b", "c", "d"}->remove_at(1), list{"a", "c", "d"})}

  testAll("20. remove_at", test20, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test21 = list{
    (list{"a", "b", "c", "d"}->insert_at(1, "alfa"), list{"a", "alfa", "b", "c", "d"}),
  }

  testAll("21. insert_at", test21, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test22 = list{(range(4, 9)->Belt.List.toArray, list{4, 5, 6, 7, 8, 9}->Belt.List.toArray)}

  testAll("22. range", test22, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test23 = list{(range_tail_recur(3, 11)->Belt.List.toArray, range(3, 11)->Belt.List.toArray)}

  testAll("23. range", test23, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let test24 = list{
    (
      rand_select(list{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}, 3)->Belt.List.toArray,
      list{"d", "i", "e"}->Belt.List.toArray,
    ),
  }

  Skip.testAll("24. rand_select", test24, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})
