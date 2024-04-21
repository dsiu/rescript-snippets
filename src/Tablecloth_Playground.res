@@uncurried
@@uncurried.swap

let log = Js.log
let log2 = Js.log2
open Tablecloth

Tuple2.make(3, 4)->Tuple2.toArray->log

module Point = {
  type t = Tuple2.t<int, int>
  let compare = Tuple2.compare(~f=Int.compare, ~g=Int.compare, ...)

  include Comparator.Make({
    type t = t
    let compare = compare
  })
}

let points = Set.fromArray([(0, 0), (3, 4), (6, 7)], module(Point))
points->Set.toArray->log

type animal = Cow | Pig | Sheep

type pointToAnimalType = Map.t<Point.t, animal, Point.identity>
let pointToAnimal = Map.fromArray(module(Point), [((0, 0), Cow), ((3, 4), Sheep), ((6, 7), Pig)])

let a = (x: pointToAnimalType) => x
let b = pointToAnimal->a
