// https://cs3110.github.io/textbook/chapters/modules/encapsulation.html#abstract-types
//

// 5.4.2. Abstract Types
//
// What if we wanted to modify that data structure to add an operation for the size of the stack? The easy way would be to implement it using List.length:
//

module ORIG = {
  module type LIST_STACK = {
    exception Empty
    let empty: list<'a>
    let is_empty: list<'a> => bool
    let push: ('a, list<'a>) => list<'a>
    let peek: list<'a> => 'a
    let pop: list<'a> => list<'a>

    let size: list<'a> => int
  }

  module ListStack: LIST_STACK = {
    let empty = list{}

    let is_empty = list =>
      switch list {
      | list{} => true
      | _ => false
      }

    let push = (x, list) => list{x, ...list}

    exception Empty

    let peek = list => {
      switch list {
      | list{} => raise(Empty)
      | list{x, ..._} => x
      }
    }

    let pop = list => {
      switch list {
      | list{} => raise(Empty)
      | list{_, ...s} => s
      }
    }

    let size = List.length
  }
}

// That results in a linear-time implementation of size. What if we wanted a faster,
// constant-time implementation? At the cost of a little space, we could cache the size of the
// stack. Let’s now represent the stack as a pair, where the first component of the pair is the
// same list as before, and the second component of the pair is the size of the stack:
//
module NEW = {
  module type STACK = {
    exception Empty
    type t<'a>
    let empty: t<'a>
    let is_empty: t<'a> => bool
    let push: ('a, t<'a>) => t<'a>
    let peek: t<'a> => 'a
    let pop: t<'a> => t<'a>

    let size: t<'a> => int
  }

  module ListStackCachedSize: STACK = {
    type t<'a> = (list<'a>, int)
    exception Empty

    let empty = (list{}, 0)

    let is_empty = stack =>
      switch stack {
      | (list{}, _) => true
      | _ => false
      }

    let push = (x, (stack, size)) => (list{x, ...stack}, size - 1)

    let peek = stack => {
      switch stack {
      | (list{}, _) => raise(Empty)
      | (list{x, ..._}, _) => x
      }
    }

    let pop = stack => {
      switch stack {
      | (list{}, _) => raise(Empty)
      | (list{_, ...stack}, size) => (stack, size - 1)
      }
    }

    let size = snd
  }

  module ListStack: STACK = {
    type t<'a> = list<'a>
    let empty = list{}

    let is_empty = list =>
      switch list {
      | list{} => true
      | _ => false
      }

    let push = (x, list) => list{x, ...list}

    exception Empty

    let peek = list => {
      switch list {
      | list{} => raise(Empty)
      | list{x, ..._} => x
      }
    }

    let pop = list => {
      switch list {
      | list{} => raise(Empty)
      | list{_, ...s} => s
      }
    }

    let size = List.length
  }
}
