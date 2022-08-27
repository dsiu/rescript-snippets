//
// List
//
module List = Belt.List

/**
  flatMap (ie: bind) on List
*/
let flatMapList: (List.t<'a>, 'a => List.t<'b>) => List.t<'b> = (xs, f) => {
  List.reduce(List.map(xs, f), list{}, List.concat)
}

//
// Array
//
module Array = Belt.Array

/**
  flatMap (ie: bind) on Array
 */
let flatMapArray: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  Array.reduce(Array.map(xs, f), [], Array.concat)
}

/**
 fold left on Array
 */
let foldLeftArray: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let init = xs->Array.getExn(0)
  let rest = xs->Array.sliceToEnd(1)
  rest->Array.reduce(init, f)
}

/**
  fold right on Array
 */
let foldRightArray: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let end = xs->Array.length - 1
  let init = xs->Array.getExn(end)
  let rest = xs->Array.slice(~offset=0, ~len=end)
  rest->Array.reduceReverse(init, f)
}

/**
  apply f(x,y) for each x in a and and each y in b
  returns result in array
*/
let combinationArray2: (array<'a>, array<'b>, ('a, 'b) => 'c) => array<'c> = (a, b, f) => {
  a->Array.reduce([], (acc, x) => {
    acc->Array.concat(
      b->Array.reduce([], (acc, y) => {
        acc->Array.concat([f(x, y)])
      }),
    )
  })
}

/**
  apply f(x,y) for each x in a and and each y in b ONLY if f(x,y) returns Some()
  returns result in array
*/
let combinationIfArray2: (array<'a>, array<'b>, ('a, 'b) => option<'c>) => array<'c> = (
  a,
  b,
  f,
) => {
  a->Array.reduce([], (acc, x) => {
    acc->Array.concat(
      b->Array.reduce([], (acc, y) => {
        switch f(x, y) {
        | Some(r) => acc->Array.concat([r])
        | None => acc
        }
      }),
    )
  })
}

//
// Options
//
/**
  option(a,b): returns a if a is Some(_) other wise return b
 */
let optionOr: (option<'a>, option<'a>) => option<'a> = (a, b) => {
  switch a {
  | Some(_) => a
  | None => b
  }
}

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
  fs->foldLeftArray(compose)
}
