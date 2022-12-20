// Common FP utils
//
let identity: 'a => 'a = (a: 'a) => a
let eq: ('a, 'a) => bool = (x, y) => x === y

/**
  composeU(f, g, x) = g(f(x))
 */
let composeU: ((. 'a) => 'b, (. 'b) => 'c, 'a) => 'c = (f, g, x) => g(. f(. x))

/**
  compose(f,g,x) = g(f(x))
 */
let compose: ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, x) => g(f(x))

let compose3 = (f, g, h, x) => h(g(f(x)))
let compose4 = (f, g, h, i, x) => i(h(g(f(x))))

let composeN = fs => {
  //  fs->Array.sliceToEnd(1)->Array.reduce(fs->Array.getExn(0), (a, f) => compose(a, f))
  open Stdlib_Array
  fs->foldLeft(compose)
}
