@@uncurried
@@uncurried.swap

module type Stack = {
  type t<'a>
  exception Empty

  let empty: t<'a>
  let is_empty: t<'a> => bool
  let push: ('a, t<'a>) => t<'a>
  let peek: t<'a> => 'a
  let pop: t<'a> => t<'a>
  let size: t<'a> => int
}

module ListStack: Stack = {
  type t<'a> = list<'a>
  exception Empty
  let empty = list{}
  let is_empty = s => {
    switch s {
    | list{} => true
    | _ => false
    }
  }
  let push = (x, s) => list{x, ...s}
  let peek = s => {
    switch s {
    | list{} => raise(Empty)
    | list{x, ..._} => x
    }
  }
  let pop = s => {
    switch s {
    | list{} => raise(Empty)
    | list{_, ...s} => s
    }
  }
  let size = List.length(_)
}

module ListStackCachedSize = {
  type t<'a> = (list<'a>, int)
  exception Empty
  let empty = (list{}, 0)
  let is_empty = s => {
    switch s {
    | (list{}, _) => true
    | _ => false
    }
  }
  let push = (x, (stack, size)) => (list{x, ...stack}, size + 1)
  let peek = s => {
    switch s {
    | (list{}, _) => raise(Empty)
    | (list{x, ..._}, _) => x
    }
  }
  let pop = s => {
    switch s {
    | (list{}, _) => raise(Empty)
    | (list{_, ...stack}, size) => (stack, size - 1)
    }
  }
  let size = snd
}

module CheckListStackCachedSize: Stack = ListStackCachedSize

module CustomStack: Stack = {
  type rec entry<'a> = {
    top: 'a,
    rest: t<'a>,
    size: int,
  }
  and t<'a> = S(option<entry<'a>>)
  exception Empty
  let empty = S(None)
  let is_empty = s => {
    switch s {
    | S(None) => true
    | _ => false
    }
  }
  let size = s => {
    switch s {
    | S(None) => 0
    | S(Some({size})) => size
    }
  }
  let push = (x, s) => {
    S(Some({top: x, rest: s, size: size(s) + 1}))
  }
  let peek = s => {
    switch s {
    | S(None) => raise(Empty)
    | S(Some({top})) => top
    }
  }
  let pop = s => {
    switch s {
    | S(None) => raise(Empty)
    | S(Some({rest})) => rest
    }
  }
}
let \">>=" = Belt.Option.flatMap
