@@uncurried
@@uncurried.swap

open BsBastet

module Hush = (B: Interface.BIFUNCTOR) => {
  let hush = bifunctor => B.bimap(Function.Category.id, Function.const((), _), bifunctor)
}

module Hush_Result = Hush(Result.Bifunctor)

let hush = Hush_Result.hush
