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

// Danny: my implementation of list find
let rec list_find:
  type a. (list<a>, ~f: a => bool) => option<a> =
  (type a, l: list<a>, ~f: a => bool): option<a> => {
    switch l {
    | list{} => None
    | list{x, ...t} => f(x) ? Some(x) : list_find(t, ~f)
    }
  }

list{1, 3, 5, 2}->list_find(~f=x => x > 3)->Js.log
list{1, 3, 5, 2}->list_find(~f=x => x > 10)->Js.log
list{"a", "B", "C"}->list_find(~f=x => x === x->Js.String2.toUpperCase)->Js.log

module If_not_found_1 = {
  // But this approach is limited to simple dependencies between types that correspond to how data
  // flows through your code. Sometimes you want types to vary in a more flexible way.

  // To make this concrete, let’s say we wanted to create a version of find that is configurable in
  // terms of how it handles the case of not finding an item

  // Let’s try to write a function that exhibits these behaviors without using GADTs. First, we’ll
  // create a variant type that represents the three possible behaviors.

  module If_not_found_1 = {
    type t<'a> =
      | Raise
      | Return_none
      | Default_to('a)
  }

  // Now we can write flexible_find, which takes an If_not_found.t as a parameter and varies its
  // behavior accordingly.

  let rec flexible_find_1 = (l, ~f, if_not_found: If_not_found_1.t<_>) => {
    switch l {
    | list{hd, ...tl} => f(hd) ? Some(hd) : flexible_find_1(tl, ~f, if_not_found)
    | list{} =>
      switch if_not_found {
      | Raise => failwith("Element not found")
      | Return_none => None
      | Default_to(x) => Some(x)
      }
    }
  }

  list{1, 2, 5}->flexible_find_1(~f=x => x > 10, Return_none)->Js.log2("flexible_find_1", _)
  list{1, 2, 5}->flexible_find_1(~f=x => x > 10, Default_to(10))->Js.log2("flexible_find_1", _)
  try {
    list{1, 2, 5}->flexible_find_1(~f=x => x > 10, Raise)->Js.log2("flexible_find_1", _)
  } catch {
  | e => Js.log2("flexible_find_1", e)
  }
  list{1, 2, 20}->flexible_find_1(~f=x => x > 10, Raise)->Js.log2("flexible_find_1", _)
}

module If_not_found_2 = {
  // This mostly does what we want, but the problem is that flexible_find always returns an option,
  // even when it’s passed Raise or Default_to, which guarantees that the None case is never used.

  // To eliminate the unnecessary option in the Raise and Default_to cases, we’re going to turn
  // If_not_found.t into a GADT. In particular, we’ll mint it as a GADT with two type parameters:
  // one for the type of the list element, and one for the return type of the function.

  module If_not_found_2 = {
    type rec t<_, _> =
      | Raise: t<'a, 'a>
      | Return_none: t<'a, option<'a>>
      | Default_to('a): t<'a, 'a>
  }

  let rec flexible_find_2:
    type a b. (~f: a => bool, list<a>, If_not_found_2.t<a, b>) => b =

    //        | If_not_found.Default_to(x) => x
    (~f, list, if_not_found) => {
      open If_not_found_2
      switch list {
      | list{} =>
        switch if_not_found {
        | Raise => failwith("No matching item found")
        | Return_none => None
        | Default_to(x) => x
        }
      | list{hd, ...tl} =>
        f(hd)
          ? {
              switch if_not_found {
              | Raise => hd
              | Return_none => Some(hd)
              | Default_to(_) => hd
              }
            }
          : flexible_find_2(~f, tl, if_not_found)
      }
    }

  // As you can see from the signature of flexible_find, the return value now depends on the type
  // of If_not_found.t, which means it can depend on the particular variant of If_not_found.t
  // that’s in use. As a result, flexible_find only returns an option when it needs to.

  flexible_find_2(~f=x => x > 10, list{1, 2, 5}, Return_none)->Js.log2("flexible_find_2", _)
  flexible_find_2(~f=x => x > 10, list{1, 2, 5}, Default_to(10))->Js.log2("flexible_find_2", _)
  try {
    flexible_find_2(~f=x => x > 10, list{1, 2, 5}, Raise)->Js.log2("flexible_find_2", _)
  } catch {
  | e => Js.log2("flexible_find_2", e)
  }
  flexible_find_2(~f=x => x > 10, list{1, 2, 20}, Raise)->Js.log2("flexible_find_2", _)
}
