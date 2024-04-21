//
// ref: https://fsharpforfunandprofit.com/posts/function-composition/
//

@@uncurried
@@uncurried.swap

open Stdlib
let log = Js.log
let compose = (. f, g, a) => g(f(a))

let add = (n, x) => x + n
let times = (n, x) => x * n

let add1Times2 = compose(add(1, _), times(2, _))
let add5Times3 = compose(add(5, _), times(3, _))

1->add1Times2->log
2->add5Times3->log

let twice = f => compose(f, f)

let add1 = add(1, _)
let add1Twice = add1->twice

9->add1Twice->log

let add1ThenMultiply = compose(add1, times(_, ...))
add1ThenMultiply(2)(7)->log

let f = (. x, y, z) => x + y * z
let g = (. (x, y, z)) => x + y * z

log("f")
2->(1->f)(3)->log
log("g")
(1, 2, 3)->g->log

let h = (. x: int, y: bool, z: float) => {
  x->ignore
  y->ignore
  z->ignore
}

let h1 = 1->h
let h2 = true->h1
let h3 = 1.0->h2
