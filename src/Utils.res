@@uncurried
@@uncurried.swap

open Stdlib

/**
 Printable
 */
module Printable = {
  /**
    MapString
    @param: m
  */
  module MapString = {
    let toString = (m, f) => {
      Belt.Map.String.reduce(m, "", (a, k, v) => {
        a ++ `key:${k}, val:${v->f}\n`
      })
    }
    module String = {
      let toString = m => toString(m, Fn.identity)
    }

    module Int = {
      let _ = Stdlib.Int.toString(1)
      let toString = (m, ~radix=10) => toString(m, Stdlib.Int.toString(_, ~radix))
    }
  }

  /**
    MapInt
  */
  module MapInt = {
    let toString = (m, f) => {
      Belt.Map.Int.reduce(m, "", (a, k, v) => {
        a ++ `key:${k->Int.toString}, val:${f(v)}\n`
      })
    }
    module String = {
      let toString = m => toString(m, Fn.identity)
    }

    module Int = {
      /**
       @returns String
       @param {Map} m the `map`
       @param b the other
      */
      let toString = (m, ~radix=10) => toString(m, Stdlib.Int.toString(_, ~radix))
    }

    module BigInt = {
      let toString = (m, ~radix=10) => toString(m, BigInt.toString(_, ~radix))
    }
  }

  // mutable map int
  module MutableMapInt = {
    let toString = (m, f) =>
      Belt.MutableMap.Int.reduce(m, "", (a, k, v) => {
        a ++ `key:${k->Int.toString}, val:${v->f}\n`
      })

    module Int = {
      let toString = (m, ~radix=10) => toString(m, Int.toString(_, ~radix))
    }

    module BigInt = {
      let toString = (m, ~radix=10) => toString(m, BigInt.toString(_, ~radix))
    }
  }

  // mutable map string
  module MutableMapString = {
    let toString = (m, f) =>
      Belt.MutableMap.String.reduce(m, "", (a, k, v) => {
        a ++ `key:${k}, val:${v->f}\n`
      })
    module Int = {
      let toString = (m, ~radix=10) => toString(m, Int.toString(_, ~radix))
    }
    module BigInt = {
      let toString = (m, ~radix=10) => toString(m, BigInt.toString(_, ~radix))
    }
  }

  module Array = {
    let toString = (a, f) => {
      "[" ++ a->Array.map(f)->Array.join(",") ++ "]"
    }
  }

  module List = {
    let toString = (a, f) => {
      a->List.reduce("{", (a, v) => a ++ f(v) ++ ",") ++ "}"
    }
  }
}

//
// Int / Int64
//
//@scope("Math") @val
@val
external parseInt: (~x: string, ~base: int) => int = "parseInt"
let base2 = Int.toString(_, ~radix=2)

let compose = (f, g) => Stdlib.Function.compose(f, g, ...)

let intFromStringExn =
  compose(String.trim, compose(Int.fromString(~radix=10, ...), Option.getUnsafe, ...), ...)

let add = (x, y) => x + y
let sub = (x, y) => x - y
let mul = (x, y) => x * y
let div = (x, y) => x / y

// Unsigned Int conversion
let int32ToUint32 = x => {
  Js.TypedArray2.Uint32Array.make([x])->Js.TypedArray2.Uint32Array.unsafe_get(0)
}

// should use BigInt
let increaseByBigInt = (v, n) => {
  v->Option.mapOr(n, x => x->BigInt.add(n))
}

let increaseBy1L = increaseByBigInt(_, 1->BigInt.fromInt)

let increaseBy = (v, n) => {
  v->Option.mapOr(n, x => x + n)
}

let increaseBy1 = increaseBy(_, 1)

//
// strings
//
let splitChars = String.split(_, "")
let splitSpace = String.split(_, " ")
let splitNewline = String.split(_, "\n")
let splitDoubleNewline = String.split(_, "\n\n")

//
// array
//

// todo: should use Array.sum(Module(Int))
let sumIntArray = Array.reduce(_, 0, add)
let mulIntArray = Array.reduce(_, 1, mul)
let join = Array.join(_, "")

// sum up elements of array from ~offset with ~len (same as Array.slice)
let sumRange = (xs, ~offset, ~len) => {
  let elems = xs->Belt.Array.slice(~offset, ~len)
  let total = ref(0)
  elems->Array.forEach(x => total := total.contents + x)
  total.contents
}

let maxIntInArray = xs => {
  let sorted = xs->Array.toSorted(Int.compare)
  sorted->Array.getUnsafe(sorted->Array.length - 1)
}

let minIntInArray = xs => {
  let sorted = xs->Array.toSorted(Int.compare)
  sorted->Array.getUnsafe(0)
}

module BigIntExt = {
  let compare = (a: bigint, b: bigint) =>
    a < b ? Ordering.less : a > b ? Ordering.greater : Ordering.equal
}

let maxBigIntInArray = xs => {
  let sorted = xs->Array.toSorted(BigIntExt.compare)
  sorted->Array.getUnsafe(sorted->Array.length - 1)
}

let minBigIntInArray = xs => {
  let sorted = xs->Array.toSorted(BigIntExt.compare)
  sorted->Array.getUnsafe(0)
}

let flatten = (xs: array<array<'a>>) => {
  xs->Array.reduce([], (a, x) => [...a, ...x])
}

// ref: https://blog.shaynefletcher.org/2017/08/transpose.html
// ref: https://github.com/nyinyithann/rescript-js-array2-extension/blob/main/src/JsArray2Ex.res

// let transpose = JsArray2Ex.transpose

let maxKeyIntValuePair = Array.reduce(_, ("", min_int), (acc, (k, v)) => {
  let (_, va) = acc
  v > va ? (k, v) : acc
})

let minKeyIntValuePair = Array.reduce(_, ("", max_int), (acc, (k, v)) => {
  let (_, va) = acc
  v < va ? (k, v) : acc
})

let keyCompareBigIntValuePair = (xs, cmp) => {
  let first = xs[0]
  let rest = Array.sliceToEnd(xs, ~start=1)

  first->Option.map(((_, init)) =>
    rest->Array.reduce(("", init), (acc, (k, v)) => {
      let (_, va) = acc
      BigInt.compare(v, va)->cmp ? (k, v) : acc
    })
  )
}

let maxKeyBigIntValuePair = keyCompareBigIntValuePair(_, Stdlib.Ordering.isGreater)
let minKeyBigIntValuePair = keyCompareBigIntValuePair(_, Stdlib.Ordering.isLess)

//
// Map / HashMap
//

let hashMapStringUpdate = (h, k, f) => {
  h->Belt.HashMap.String.set(
    k,
    h->Belt.HashMap.String.get(k)->Option.mapOr(f(None), x => f(Some(x))),
  )
  h
}

let mutableMapStringUpdate = (h, k, f) => {
  h->Belt.MutableMap.String.set(
    k,
    h->Belt.MutableMap.String.get(k)->Option.mapOr(f(None), x => f(Some(x))),
  )
  h
}
