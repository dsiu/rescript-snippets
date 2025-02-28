open StdlibFp

module M: OP_Set.S = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = (x, xs) => List.has(xs, x, (a, b) => a == b)
  let add = (x, xs) => List.cons(xs, x)
  let elements = xs => List.sort(xs, (a, b) => Pervasives.compare(a, b)->Ordering.fromInt)
}
