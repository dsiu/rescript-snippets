let log = Js.log
let log2 = Js.log2
open Stdlib

module TC = Tablecloth

TC.Tuple2.make(3, 4)->TC.Tuple2.toArray->log

module Point = {
  type t = TC.Tuple2.t<int, int>

  let intCompare = (a, b) => Int.compare(a, b)->Stdlib.Ordering.toInt

  let compare = TC.Tuple2.compare(~f=intCompare, ~g=intCompare, ...)

  include TC.Comparator.Make({
    type t = t
    let compare = compare
  })
}

let points = TC.Set.fromArray([(0, 0), (3, 4), (6, 7)], module(Point))
points->TC.Set.toArray->log

type animal = Cow | Pig | Sheep

type pointToAnimalType = TC.Map.t<Point.t, animal, Point.identity>
let pointToAnimal = TC.Map.fromArray(module(Point), [((0, 0), Cow), ((3, 4), Sheep), ((6, 7), Pig)])

let a = (x: pointToAnimalType) => x
let b = pointToAnimal->a
