//
// https://wiki.haskell.org/OOP_vs_type_classes
//

let log = Js.log

//
// Type with several representations: use algebraic data type (ADT)
//
// For the types with different representations, algebraic data types (ADT) - an analog of
// discriminated unions - are supported
//
type point =
  | FloatPoint(float, float)
  | IntPoint(int, int)

let coord = p =>
  switch p {
  | FloatPoint(x, y) => (x, y)
  | IntPoint(x, y) => (float_of_int(x), float_of_int(y))
  }

FloatPoint(1.0, 2.0)->coord->log
IntPoint(3, 4)->coord->log

//
// Packing data & functions together: use (records of) closures
//
let inc = a => {a := a.contents + 1}
let read = a => a.contents

let proc = (inc, read) => {
  x => {
    inc(x)
    inc(x)
    inc(x)
    read(x)->log
  }
}

proc(inc, read)(ref(5))

//
// Hiding implementation details: use module export list
// => use module interfaces .resi file)
//

//
// Grouping related functionality: use module hierarchy and Haddock markup
// => use module hierarchy
//
