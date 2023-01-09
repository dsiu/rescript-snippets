open Stdlib
module L = List

let log = Js.log
let log2 = Js.log2
open Belt

module Fold_And_Monoids = {
  module type Monoid = {
    type t
    let combine: (t, t) => t
    let empty: t
  }

  module IntMonoid: Monoid with type t = int = {
    type t = int
    let combine = (m, n) => m + n
    let empty = 0
  }

  let lst = Belt.List.makeBy(100, i => i)
  lst->List.reduce(IntMonoid.empty, IntMonoid.combine)->log2(__LINE__, _)

  module StringMonoid: Monoid with type t = string = {
    type t = string
    let combine = (m, n) => m ++ n
    let empty = ""
  }

  let lst = list{"a", "b", "c", "d"}
  lst->List.reduce(StringMonoid.empty, StringMonoid.combine)->log2(__LINE__, _)

  module ProductMonoid: Monoid with type t = float = {
    type t = float
    let combine = (m, n) => m *. n
    let empty = 1.0
  }

  let lst = list{1.0, 2.0, 3.0, 4.0}
  lst->List.reduce(ProductMonoid.empty, ProductMonoid.combine)->log2(__LINE__, _)
}

module More_Higher_Order_Functions = {
  let sumOfSquares = n => {
    open List
    makeBy(n, i => i + 1)->map(m => m * m)->reduce(0, \"+")
  }
  sumOfSquares(10)->log2(__LINE__, _)
}

module From_Map_To_FlatMap = {
  let ls = list{"Hello", "World"}
  ls->L.flatMap(c => list{c->Js.String2.toUpperCase})->List.toArray->log2(__LINE__, _)
}
