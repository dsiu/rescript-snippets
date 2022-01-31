//
// Async/await pattern matching
//
// ref:
// https://forum.rescript-lang.org/t/alternate-async-await-proposal/2786
//

@val external _await: Js.Promise.t<'a> => 'a = "await"

@val external _async0: (unit => 'a, unit) => Js.Promise.t<'a> = "async"
@val external _async1: ('a => 'b, 'a) => Js.Promise.t<'b> = "async"
@val external _async2: (('a, 'b) => 'c, 'a, 'b) => Js.Promise.t<'c> = "async"

let asyncGetInt = _async1((num: int) => num)
let asyncSum = _async2((x: int, y: int): string => string_of_int(x + y))

let asyncProg = _async0(() => {
  let a = _await(asyncGetInt(5))
  let b = _await(asyncGetInt(10))
  let c = _await(asyncSum(a, b))
  Js.log((c: string))
})

//Js.Promise.then_(asyncProg, ()->Js.Promise.resolve)->ignore
