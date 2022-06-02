//
// Real World OCaml - Ch.9 GADTs
// https://dev.realworldocaml.org/gadts.html
//

@@warning("-26")

// When Are GADTs Useful?

// The typed language we showed above is a perfectly reasonable example, but GADTs are useful for
// a lot more than designing little languages. In this section, we’ll try to give you a broader
// sampling of the kinds of things you can do with GADTs.

//
// Varying Your Return Type
//

// Sometimes, you want to write a single function that can effectively have different types in
// different circumstances. In some sense, this is totally ordinary. After all, OCaml’s
// polymorphism means that values can take on different types in different contexts. List.find
// is a fine example. The signature indicates that the type of the result varies with the type
// of the input list.

// Danny: my implementation of list flnd
let rec list_find:
  type a. (list<a>, a => bool) => option<a> =
  (type a, l: list<a>, f: a => bool): option<a> => {
    switch l {
    | list{} => None
    | list{x, ...t} => f(x) ? Some(x) : list_find(t, f)
    }
  }

list{1, 3, 5, 2}->list_find(x => x > 3)->Js.log
list{1, 3, 5, 2}->list_find(x => x > 10)->Js.log
list{"a", "B", "C"}->list_find(x => x === x->Js.String2.toUpperCase)->Js.log

// But this approach is limited to simple dependencies between types that correspond to how data
// flows through your code. Sometimes you want types to vary in a more flexible way.

// To make this concrete, let’s say we wanted to create a version of find that is configurable in
// terms of how it handles the case of not finding an item

// Let’s try to write a function that exhibits these behaviors without using GADTs. First, we’ll
// create a variant type that represents the three possible behaviors.

module If_not_found = {
  type t<'a> =
    | Raise
    | Return_none
    | Default_to('a)
}

// Now we can write flexible_find, which takes an If_not_found.t as a parameter and varies its
// behavior accordingly.

let rec flexible_find = (l, f, if_not_found: If_not_found.t<_>) => {
  switch l {
  | list{hd, ...tl} => f(hd) ? Some(hd) : flexible_find(tl, f, if_not_found)
  | list{} =>
    switch if_not_found {
    | Raise => raise(Not_found)
    | Return_none => None
    | Default_to(x) => Some(x)
    }
  }
}
