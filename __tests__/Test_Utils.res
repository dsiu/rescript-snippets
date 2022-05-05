open Test

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let boolEqual = (~message=?, a: bool, b: bool) =>
  assertion(~message?, ~operator="boolEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

let stringMapEqual = (~message=?, a, b) =>
  assertion(
    ~message?,
    ~operator="stringMapEqual",
    (a, b) => Belt.Map.String.eq(a, b, (a, b) => a === b),
    a,
    b,
  )

let stringArrayEqual = (~message=?, a, b) =>
  assertion(
    ~message?,
    ~operator="stringArrayEqual",
    (a, b) => Belt.Array.eq(a, b, (a, b) => a === b),
    a,
    b,
  )

let intArrayEqual = (~message=?, a, b) =>
  assertion(
    ~message?,
    ~operator="intArrayEqual",
    (a, b) => Belt.Array.eq(a, b, (a, b) => a === b),
    a,
    b,
  )

let listEqual = (~message=?, a, b) =>
  assertion(~message?, ~operator="listEqual", (a, b) => Belt.List.eq(a, b, (a, b) => a === b), a, b)

let optionEqual = (~message=?, a, b) =>
  assertion(
    ~message?,
    ~operator="optionEqual",
    (a, b) => Belt.Option.eq(a, b, (a, b) => a === b),
    a,
    b,
  )

let optionListEqual = (~message=?, a, b) =>
  assertion(
    ~message?,
    ~operator="optionEqual",
    (a, b) => Belt.Option.eq(a, b, (a, b) => Belt.List.eq(a, b, (a, b) => a === b)),
    a,
    b,
  )
