@@uncurried
@@uncurried.swap

module M: OP_Stack.S = {
  type t<'a> = list<'a>
  exception Empty
  let empty = list{}
  let is_empty = x =>
    switch x {
    | list{} => true
    | _ => false
    }
  let push = List.cons
  let peek = x =>
    switch x {
    | list{} => raise(Empty)
    | list{x, ..._} => x
    }
  let pop = x =>
    switch x {
    | list{} => raise(Empty)
    | list{_, ...s} => s
    }
  let size = List.length
  let to_list = Stdlib.Function.identity
}
