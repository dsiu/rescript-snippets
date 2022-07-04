module M: OP_Set.S = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = List.mem
  let add = (x, s) =>
    if mem(x, s) {
      s
    } else {
      list{x, ...s}
    }
  let elements = FP_Utils.id
}
