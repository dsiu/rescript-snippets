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
  | _ => Js.log2("flexible_find_1", "nothing > 10")
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
  | _ => Js.log2("flexible_find_2", "nothing > 10")
  }
  flexible_find_2(~f=x => x > 10, list{1, 2, 20}, Raise)->Js.log2("flexible_find_2", _)
}

//
// Capturing the Unknown
//
// Code that works with unknown types is routine in OCaml, and comes up in the simplest of
// examples:

module Capturing_The_Unknown = {
  "Capturing_The_Unknown"->Js.log

  let tuple_i_f: (int, float) => (int, float) = (x, y) => (x, y)
  let tuple_s_s: (string, string) => (string, string) = (x, y) => (x, y)

  // Sometimes, however, we want type variables that are existentially quantified, meaning that
  // instead of being compatible with all types, the type represents a particular but unknown type.
  type rec stringable = Stringable({value: 'a, to_string: 'a => string}): stringable

  // This type packs together a value of some arbitrary type, along with a function for converting
  // values of that type to strings.

  // We can tell that 'a is existentially quantified because it shows up on the left-hand side of the
  // arrow but not on the right, so the 'a that shows up internally doesn’t appear in a type
  // parameter for stringable itself. Essentially, the existentially quantified type is bound within
  // the definition of stringable.

  // The following function can print an arbitrary stringable:
  let print_stringable = (Stringable(s)) => Js.log(s.to_string(s.value))

  let id = x => x

  let stringables = {
    let s = (value, to_string) => Stringable({value: value, to_string: to_string})
    list{s(100, Belt.Int.toString), s(12.3, Belt.Float.toString), s("foo", id)}
  }

  // polymorphic print!!
  Belt.List.map(stringables, print_stringable)->ignore

  // The thing that lets this all work is that the type of the underlying object is existentially
  // bound within the type stringable. As such, the type of the underlying values can’t escape the
  // scope of stringable, and any function that tries to return such a value won’t type-check.

  // let get_value = (Stringable(s)) => s.value // this wont' compile

  // Error: This expression has type \"$Stringable_'a"
  //       but an expression was expected of type 'a
  //       The type constructor $Stringable_'a would escape its scope

  // It’s worth spending a moment to decode this error message, and the meaning of the type
  // variable $Stringable_'a in particular. You can think of this variable as having three parts:

  //  * The $ marks the variable as an existential.
  //  * Stringable is the name of the GADT tag that this variable came from.
  //  * 'a is the name of the type variable from inside that tag.
}

module Abstracting_Computational_Machines = {
  // A common idiom in OCaml is to combine small components into larger computational
  // machines, using a collection of component-combining functions, or combinators.

  // GADTs can be helpful for writing such combinators. To see how, let’s consider an example:
  // pipelines. Here, a pipeline is a sequence of steps where each step consumes the output of the
  // previous step, potentially do2es some side effects, and returns a value to be passed to the
  // next step. This is analogous to a shell pipeline, and is useful for all sorts of system
  // automation tasks.

  // But, can’t we write pipelines already? After all, OCaml comes with a perfectly serviceable
  // pipeline operator:
  type stats = {st_size: int}
  let ls_dir = list{"d1", "f111", "f222", "d2", "f333"}
  let is_file_exn = s => s->String.get(0) === 'f'
  let lstat = s => {
    {
      st_size: s
      ->Js.String2.substringToEnd(~from=1)
      ->Belt.Int.fromString
      ->Belt.Option.getWithDefault(0),
    }
  }
  let list_sum = l => l->Belt.List.reduce(0, (a, x) => a + x)

  let sum_file_sizes = () => {
    ls_dir->Belt.List.keep(is_file_exn)->Belt.List.map(x => {x->lstat}.st_size)->list_sum
  }

  sum_file_sizes()->Js.log

  type rec pipeline<_, _> =
    | Step('a => 'b, pipeline<'b, 'c>): pipeline<'a, 'c>
    | Empty: pipeline<'a, 'a>

  let add_step = (pipeline, f) => Step(pipeline, f)
  let \"+" = add_step
  let empty = Empty

  let rec exec:
    type a b. (pipeline<a, b>, a) => b =
    (pipeline, input) => {
      switch pipeline {
      | Empty => input
      | Step(f, tail) => exec(tail, f(input))
      }
    }

  let p1 = add_step(
    () => ls_dir,
    add_step(
      Belt.List.keep(_, is_file_exn),
      add_step(Belt.List.map(_, x => {x->lstat}.st_size), add_step(list_sum, Empty)),
    ),
  )

  Js.log("using pipeline GADT p1")
  exec(p1)()->Js.log

  let exec_with_profile = (pipeline, input) => {
    let rec loop:
      type a b. (pipeline<a, b>, a, list<int>) => (b, list<int>) =
      (pipeline, input, rev_profile) =>
        switch pipeline {
        | Empty => (input, rev_profile)
        | Step(f, tail) => {
            let start = 0
            let output = f(input)
            let elapsed = 13
            loop(tail, output, list{elapsed, ...rev_profile})
          }
        }
    let (output, rev_profile) = loop(pipeline, input, list{})
    (output, List.rev(rev_profile))
  }

  Js.log("using pipeline GADT with profile p1")
  exec_with_profile(p1)()->Js.log
}

//
// Narrowing the Possibilities
//
// A COMPLETION-SENSITIVE OPTION TYPE
//
module Narrowing_the_Possibilities = {
  type incomplete = Incomplete
  type complete = Complete

  type rec coption<_, _> =
    | Absent: coption<_, incomplete>
    | Present('a): coption<'a, _>
}
