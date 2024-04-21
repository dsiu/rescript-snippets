@@uncurried
@@uncurried.swap

module M: OP_Set.S = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = List.mem
  let add = List.cons
  let elements = s => List.sort_uniq(Pervasives.compare, s)
}
