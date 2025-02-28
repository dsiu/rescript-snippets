@@uncurried
@@uncurried.swap

module M: OP_Set.S = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = (x, xs) => List.has(xs, x, (a, b) => a == b)

  let add = (x, s) =>
    if mem(x, s) {
      s
    } else {
      list{x, ...s}
    }
  let elements = StdlibFp.Function.identity
}
