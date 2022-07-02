module M: OP_Queue.S = {
  @ocaml.doc(" The list [x1; x2; ...; xn] represents the queue with [x1] at its front,
      followed by [x2], ..., followed by [xn]. ")
  type t<'a> = list<'a>
  exception Empty
  let empty = list{}
  let is_empty = x =>
    switch x {
    | list{} => true
    | _ => false
    }
  let enqueue = (x, q) => \"@"(q, list{x})
  let front = x =>
    switch x {
    | list{} => raise(Empty)
    | list{x, ..._} => x
    }
  let dequeue = x =>
    switch x {
    | list{} => raise(Empty)
    | list{_, ...q} => q
    }
  let size = List.length
  let to_list = FP_Utils.id
}
