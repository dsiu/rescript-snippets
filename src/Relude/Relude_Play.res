let log = Js.log
let log2 = Js.log2

// Reader

type env = {
  intValue: int,
  stringValue: string,
}

let testEnv: env = {intValue: 42, stringValue: "abc"}

type error = {message: string}

module ReaderT = Relude.ReaderT

module Reader = Relude.Reader.WithEnv({
  type t = env
})

let r = Reader.make(r => r.intValue * 2) |> Reader.runReaderT(testEnv)
r->log

// Array of Tuples
module Tuple = Relude.Tuple
module Array = Relude.Array
module String = Relude.String
module Int = Relude.Int

module AT_StrInt = {
  include Relude_Array_Instances
  include Relude_Array_Base

  include Array.ArrayOrdExtensions(Tuple.WithOrds2(Int.Ord, String.Ord))
}

let arr = [(3, "c"), (26, "z"), (1, "a"), (2, "b")]
let sorted = arr->AT_StrInt.sort
arr->log
sorted->log

let plus1 = ((i, s)) => AT_StrInt.pure((i + 1, s ++ s))
let reverseTuple = ((i, s)) => AT_StrInt.pure((s, i))

arr->AT_StrInt.bind(plus1)->log
let arr_rev = arr->AT_StrInt.bind(reverseTuple)

module AT_IntStr = {
  include Relude_Array_Instances
  include Relude_Array_Base

  include Array.ArrayOrdExtensions(Tuple.WithOrds2(String.Ord, Int.Ord))
}
arr_rev->AT_IntStr.sort->log

arr_rev->AT_IntStr.showBy(Tuple.showBy2(String.show, Int.show), _)->log
