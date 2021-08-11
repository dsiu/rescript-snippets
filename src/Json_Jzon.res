// Json encoding and decoding with https://github.com/nkrkv/jzon
// Jzon is a library for ReScript to encode and decode JSON data with type safety.

// Imagine you have the following ReScript types to encode and decode:
type style = {
  size: float,
  color: string,
}

type point = {
  x: float,
  y: float,
  z: float,
  style: option<style>,
}

// First, define their codecs:
module Codecs = {
  let style = Jzon.object2(
    ({size, color}) => (size, color),
    ((size, color)) => {size: size, color: color}->Ok,
    Jzon.field("size", Jzon.float),
    Jzon.field("color", Jzon.string),
  )

  let point = Jzon.object4(
    ({x, y, z, style}) => (x, y, z, style),
    ((x, y, z, style)) => {x: x, y: y, z: z, style: style}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("z", Jzon.float)->Jzon.default(0.0),
    Jzon.field("style", style)->Jzon.optional,
  )
}

// Next, convert between the ReScript types and Js.Json.t with:
let myJsonData = Codecs.point->Jzon.encode({
  x: 1.0,
  y: 2.0,
  z: 3.0,
  style: Some({size: 4.0, color: "#fd0"}),
})

// and back with:
let myPoint = Codecs.point->Jzon.decode(myJsonData)

"Json"->Js.log
myJsonData->Js.log

"ReScript Type"
myPoint->Js.log
