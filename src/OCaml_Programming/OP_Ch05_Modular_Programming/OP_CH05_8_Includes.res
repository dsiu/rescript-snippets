// https://cs3110.github.io/textbook/chapters/modules/includes.html
//
//

// The OCaml module system provides a neat feature called includes that is like a principled
// copy-and-paste that is quick and easy to use, but avoids actual duplication. It can be used
// to solve some of the same problems as inheritance in object-oriented languages.

@@uncurried
@@uncurried.swap

module type Set = {
  type t<'a>
  let empty: t<'a>
  let mem: (. 'a, t<'a>) => bool
  let add: (. 'a, t<'a>) => t<'a>
  let elements: t<'a> => list<'a>
}

module ListSet: Set = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = List.mem
  let add = List.cons
  let elements = s => List.sort_uniq(Pervasives.compare, s)
}

// Suppose we wanted to add a function of_list : 'a list -> 'a t that could construct a set out
// of a list. If we had access to the source code of both ListSet and Set, and if we were
// permitted to modify it, this wouldn’t be hard. But what if they were third-party libraries
// for which we didn’t have source code?

// OCaml includes are similar. They enable a module to include all the items defined by
// another module, or a module type to include all the specifications of another module type.
//
//Here’s how we can use includes to solve the problem of adding of_list to ListSet:
//
module ListSetExtended_ = {
  include ListSet
  let of_list = lst => List.fold_right(add, lst, empty)
}

// 5.8.1. Semantics of Includes
//
// Includes can be used inside of structures and signatures. When we include inside a signature,
// we must be including another signature. And when we include inside a structure, we must
// be including another structure.

module type SetExtended = {
  include Set
  let of_list: list<'a> => t<'a>
}

module ListSetExtended: SetExtended = {
  include ListSet
  let of_list = lst => List.fold_right(add, lst, empty)
}

// 5.8.2. Encapsulation and Includes
//

// Ah, now the problem is clearer: in the body of of_list, the equality of 'a t and 'a list
// isn’t known. In ListSetExtended, we do know that 'a t = 'a ListSet.t, because that’s what
// the include gave us. But the fact that 'a ListSet.t = 'a list was hidden when ListSet was
// sealed at module type Set. So, includes must obey encapsulation, just like the rest of the
// module system.

// One workaround is to rewrite the definitions as follows:
//

module Work_Around = {
  module ListSetImpl = {
    type t<'a> = list<'a>
    let empty = list{}
    let mem = List.mem
    let add = List.cons
    let elements = s => List.sort_uniq(Pervasives.compare, s)
  }

  module ListSet: Set = ListSetImpl

  module type SetExtended = {
    include Set
    let of_list: list<'a> => t<'a>
  }

  module ListSetExtendedImpl = {
    include ListSetImpl
    let of_list = lst => lst
  }

  module ListSetExtended: SetExtended = ListSetExtendedImpl
}

// The important change is that ListSetImpl is not sealed, so its type 'a t is not abstract.
// When we include it in ListSetExtended, we can therefore exploit the fact that it’s a synonym
// for 'a list.
//
// What we just did is effectively the same as what Java does to handle the visibility
// modifiers public, private, etc. The “private version” of a class is like the Impl version
// above: anyone who can see that version gets to see all the exposed items (fields in Java,
// types in OCaml), without any encapsulation. The “public version” of a class is like the
// sealed version above: anyone who can see that version is forced to treat the items as
// abstract, hence encapsulated.
//

// With that technique, if we want to provide a new implementation of one of the included
// functions we could do that too:
module ListSetExtendedImpl = {
  include Work_Around.ListSetImpl
  let of_list = lst => List.fold_right(add, lst, empty)
  let rec elements = l => {
    switch l {
    | list{} => list{}
    | list{h, ...t} => mem(h, t) ? elements(t) : list{h, ...elements(t)}
    }
  }
}

// 5.8.4. Including Code in Multiple Modules
//
