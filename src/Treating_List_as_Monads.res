//
// Treating Lists as Monads — HaskellRank Ep.11
//
// https://www.youtube.com/watch?v=ofUAlkYHFsI&list=PLr42FYa8kCv--Rtt_r7WCU5J-7qpnkZ76&index=7&t=6s
//

@@uncurried
@@uncurried.swap

open StdlibFp
let log = Js.log

module A = Array
module O = Option
let flip = Function.flip
let sort = Belt.SortArray.stableSortBy
let lessThanEq = (a, b) => a <= b

let b = 10
let keyboards = [3, 1]
let drives = [5, 2, 8]
let pair = (a, b) => (a, b)

A.liftM2(pair, keyboards, drives)->log

let solve = (b, keyboards, drives) => {
  A.liftM2(\"+", keyboards, drives)
  ->A.filter(lessThanEq(_, b))
  ->sort(flip(compare, ...))
  ->O.arrayToMayBe
  ->O.fromMaybe(-1)
}

solve(b, keyboards, drives)->log
