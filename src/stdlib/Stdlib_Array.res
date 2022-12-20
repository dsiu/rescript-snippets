include Belt.Array

// Basic Functions
let append = concat
let head = getExn(_, 0)
let last = xs => getExn(xs, xs->length - 1)
let tail = sliceToEnd(_, 1)
let init = xs => {
  let l = length(xs)
  l == 0 ? None : Some(Belt.Array.slice(xs, ~offset=0, ~len=l - 1))
}

let uncons = xs =>
  switch xs {
  | [] => None
  | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
  }

let singleon = make(1, _)

let take = (xs, n) => {
  let l = length(xs)
  let len = n < 0 ? 0 : l < n ? l : n
  Belt.Array.slice(xs, ~offset=0, ~len)
}

let takeExactly = (xs, n) =>
  n < 0 || n > length(xs) ? None : Some(Belt.Array.slice(xs, ~offset=0, ~len=n))

let takeWhile = (xs, predicateFn) =>
  Belt.Array.reduceU(xs, [], (. acc, element) => {
    if predicateFn(element) {
      Js.Array2.push(acc, element)->ignore
    }
    acc
  })

let drop = (xs, n) => {
  let l = length(xs)
  let start = n < 0 ? 0 : l < n ? l : n
  Belt.Array.sliceToEnd(xs, start)
}

let dropExactly = (xs, n) => n < 0 || n > length(xs) ? None : Some(Belt.Array.sliceToEnd(xs, n))

let dropWhile = (xs, predicateFn) =>
  Belt.Array.reduceU(xs, [], (. acc, element) => {
    if !predicateFn(element) {
      Js.Array2.push(acc, element)->ignore
    }
    acc
  })

let rec tails = xs => {
  xs->length == 0 ? [[]] : concat([xs], tails(tail(xs)))
}

external coerce: 'a => 'b = "%identity"

let some = (xs, fn) => Belt.Array.someU(xs, fn)

let uniqBy = (xs, uniqFn) => {
  let index = ref(0)
  let arr = []

  while index.contents < length(xs) {
    let value = Belt.Array.getUnsafe(xs, index.contents)
    let alreadyAdded = some(arr, (. x) => uniqFn(coerce(x)) == uniqFn(value))

    if !alreadyAdded {
      Js.Array2.push(arr, value)->ignore
    }

    index := succ(index.contents)
  }

  arr
}

let uniq = xs => uniqBy(xs, element => element)

let splitAt = (xs, offset) =>
  offset < 0 || offset > length(xs)
    ? None
    : Some((Belt.Array.slice(xs, ~offset=0, ~len=offset), Belt.Array.sliceToEnd(xs, offset)))

//
// scanl [dsiu]
//
// https://kapeli.com/dash_share?docset_file=Haskell&docset_name=Haskell&path=libraries/base-4.17.0.0/Data-List.html%23v:scanl&platform=haskell&repo=Main&version=9.4.3
//
// [1, 2, 3, 4]->Array.scanl(0, \"+") -> [ 0, 1, 3, 6, 10 ]
//
//  []->Array.scanl(42, \"+") -> [ 42 ]/
//
//  [1, 2, 3, 4]->Array.scanl(100, \"-") -> [ 100, 99, 97, 94, 90 ]
//
//  ["a", "b", "c", "d"]->Array.scanl("foo", (rString, nextChar) => nextChar ++ rString)
//  -> [ 'foo', 'afoo', 'bafoo', 'cbafoo', 'dcbafoo' ]
//
let rec scanl: (array<'b>, 'a, ('a, 'b) => 'a) => array<'a> = (xs, initial, fn) => {
  concat(
    [initial],
    {
      xs->length == 0
        ? []
        : {
            let h = xs->head
            let tails = xs->tail
            tails->scanl(fn(initial, h), fn)
          }
    },
  )
}

// Array transformations

/**
  flatMap (ie: bind) on Array
 */
let flatMap: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  reduce(map(xs, f), [], concat)
}

let arrayToOption = get(_, 0)

/**
 fold left on Array
 */
let foldLeft: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let init = xs->getExn(0)
  let rest = xs->tail
  rest->reduce(init, f)
}

/**
  fold right on Array
 */
let foldRight: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let end = xs->length - 1
  let init = xs->getExn(end)
  let rest = xs->slice(~offset=0, ~len=end)
  rest->reduceReverse(init, f)
}

/**
  apply f(x,y) for each x in a and each y in b ONLY if f(x,y) returns Some()
  returns result in array
*/
let combinationIf2: (array<'a>, array<'b>, (. 'a, 'b) => option<'r>) => array<'r> = (a, b, f) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      switch f(. x, y) {
      | Some(r) => ret := ret.contents->concat([r])
      | None => ()
      }
    })
  })

  ret.contents
}

/**
  apply f(x,y) for each x in a and and each y in b
  returns result in array
*/
let combination2: (array<'a>, array<'b>, (. 'a, 'b) => 'r) => array<'r> = (a, b, f) => {
  combinationIf2(a, b, (. x, y) => Some(f(. x, y)))
}

/**
  apply f(x,y,z) for each x in a, each y in b, and each z in c ONLY if f(x,y,z) returns Some()
  returns result in array
*/
let combinationIf3: (array<'a>, array<'b>, array<'c>, (. 'a, 'b, 'c) => option<'r>) => array<'r> = (
  a,
  b,
  c,
  f,
) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      c->forEach(
        z => {
          switch f(. x, y, z) {
          | Some(r) => ret := ret.contents->concat([r])
          | None => ()
          }
        },
      )
    })
  })

  ret.contents
}

/**
  apply f(x,y,z) for each x in a, y in b, z in c
  returns result in array
*/
let combinationArray3: (array<'a>, array<'b>, array<'c>, (. 'a, 'b, 'c) => 'r) => array<'r> = (
  a,
  b,
  c,
  f,
) => {
  combinationIf3(a, b, c, (. x, y, z) => Some(f(. x, y, z)))
}

/**
  apply f(x,y,z,w) for each x in a, each y in b, each z in c, and each w in d, ONLY if f(x,y,z,w) returns Some()
  returns result in array
*/
let combinationIf4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => option<'e>,
) => array<'e> = (a, b, c, d, f) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      c->forEach(
        z => {
          d->forEach(
            w => {
              switch f(. x, y, z, w) {
              | Some(r) => ret := ret.contents->concat([r])
              | None => ()
              }
            },
          )
        },
      )
    })
  })

  ret.contents
}

/**
  apply f(x,y,z) for each x in a, y in b, z in c, w in d
  returns result in array
*/
let combination4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => 'r,
) => array<'r> = (a, b, c, d, f) => {
  combinationIf4(a, b, c, d, (. x, y, z, w) => Some(f(. x, y, z, w)))
}
