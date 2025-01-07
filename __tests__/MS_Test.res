open Jest
open Expect
open MS

open Stdlib

describe("MS", () => {
  let fromStr_Tests =
    [
      (fromStr("10h"), 36000000.),
      (fromStr("2.5 hrs"), 9000000.),
      (fromStr("2h"), 7200000.),
      (fromStr("1m"), 60000.),
      (fromStr("5s"), 5000.),
      (fromStr("1y"), 31557600000.),
      (fromStr("100"), 100.),
      (fromStr("-3 days"), -259200000.),
      (fromStr("-1h"), -3600000.),
      (fromStr("-200"), -200.),
    ]->List.fromArray

  testAll("fromStr", fromStr_Tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let fromMS_Tests =
    [
      (fromMS(60000.), "1m"),
      (fromMS(2. *. 60000.), "2m"),
      (fromMS(-3. *. 60000.), "-3m"),
      (fromMS(fromStr("10 hours")), "10h"),
    ]->List.fromArray

  testAll("fromMS", fromMS_Tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let fromMSLong_Tests =
    [
      (fromMSLong(60000.), "1 minute"),
      (fromMSLong(2. *. 60000.), "2 minutes"),
      (fromMSLong(-3. *. 60000.), "-3 minutes"),
      (fromMSLong(fromStr("10 hours")), "10 hours"),
    ]->List.fromArray

  testAll("fromMSLong", fromMSLong_Tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})
