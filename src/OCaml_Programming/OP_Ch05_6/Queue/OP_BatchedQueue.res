module M: OP_Queue.S = {
  @ocaml.doc(" [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
      is not. This implies that if [o] is empty, [i] must also be empty. ")
  type t<'a> = {o: list<'a>, i: list<'a>}

  exception Empty

  let empty = {o: list{}, i: list{}}

  let is_empty = x =>
    switch x {
    | {o: list{}} => true
    | _ => false
    }

  let enqueue = (x, q) =>
    switch q {
    | {o: list{}} => {o: list{x}, i: list{}}
    | {o, i} => {o: o, i: list{x, ...i}}
    }

  let front = x =>
    switch x {
    | {o: list{}} => raise(Empty)
    | {o: list{h, ..._}} => h
    }

  let dequeue = x =>
    switch x {
    | {o: list{}} => raise(Empty)
    | {o: list{_}, i} => {o: List.rev(i), i: list{}}
    | {o: list{_, ...t}, i} => {o: t, i: i}
    }

  let size = ({o, i}) => {
    open List
    length(o) + length(i)
  }

  let to_list = ({o, i}) => \"@"(o, List.rev(i))
}
