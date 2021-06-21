// https://forum.rescript-lang.org/t/should-i-need-to-write-a-one-to-one-bindings-if-i-want-this-bindings-publish-to-npm/1888

// -----------------------------------------------
// v1
module Interval = {
  type timeMillisecond
  type timeSecond

  @module("d3")
  external timeMillisecond: timeMillisecond = "timeMillisecond"

  @module("d3")
  external timeSecond: timeSecond = "timeSecond"

  @send
  external floor: ('t, Js.Date.t) => Js.Date.t = "floor"

  @send
  external round: ('t, Js.Date.t) => Js.Date.t = "round"

  @send
  external ceil: ('t, Js.Date.t) => Js.Date.t = "ceil"

  @send
  external offset: ('t, Js.Date.t, option<int>) => Js.Date.t = "offset"

  @send
  external range: ('t, Js.Date.t, Js.Date.t, option<int>) => array<Js.Date.t> = "range"

  @send
  external filter: ('t, Js.Date.t => bool) => 't = "filter"

  @send
  external every: ('t, int) => 't = "every"

  @send
  external count: ('t, Js.Date.t, Js.Date.t) => int = "count"
}

// -----------------------------------------------
// v2
module D3v2 = {
type time<'a>
type minute
type second
type millisecond

@module("d3") external timeMinute: time<minute> = "timeMinute"
@module("d3") external timeSecond: time<second> = "timeSecond"
@module("d3") external timeMillisecond: time<millisecond> = "timeMillisecond"

@send external floor: (time<_>, Js.Date.t) => Js.Date.t = "floor"
@send external ceil: (time<_>, Js.Date.t) => Js.Date.t = "ceil"
}
