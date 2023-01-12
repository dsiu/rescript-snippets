open BsBastet

module Hush = (B: Interface.BIFUNCTOR) => {
  let hush = bifunctor => bifunctor |> B.bimap(Function.Category.id, Function.const())
}

module Hush_Result = Hush(Result.Bifunctor)

let hush = Hush_Result.hush
