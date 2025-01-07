//
// Real World OCaml - Ch.9 GADTs
// https://dev.realworldocaml.org/gadts.html
//
@@uncurried
@@uncurried.swap

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
    let s = (value, to_string) => Stringable({value, to_string})
    list{s(100, Belt.Int.toString(_)), s(12.3, Belt.Float.toString(_)), s("foo", id)}
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
  let ls_dir = d => {
    switch d {
    | "." => list{"d1", "f111", "f222", "d2", "f333"}
    | ".." => list{"d10", "f100", "f200", "d2", "f300"}
    | _ => list{"d1000", "f11100", "f22200", "d200", "f33300"}
    }
  }

  let is_file_exn = s => s->String.getUnsafe(0) === "f"
  let lstat = s => {
    {
      st_size: s
      ->Js.String2.substringToEnd(~from=1)
      ->Belt.Int.fromString
      ->Belt.Option.getWithDefault(0),
    }
  }
  let list_sum = l => l->Belt.List.reduce(0, (a, x) => a + x)

  let sum_file_sizes = d => {
    d->ls_dir->Belt.List.keep(is_file_exn)->Belt.List.map(x => {x->lstat}.st_size)->list_sum
  }

  sum_file_sizes(".")->Js.log

  type rec pipeline<_, _> =
    | Step('a => 'b, pipeline<'b, 'c>): pipeline<'a, 'c>
    | Empty: pipeline<'a, 'a>

  let add_step = (f, pipeline) => Step(f, pipeline)
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

  // this pipeline can be used in both exec() and exec_with_profile!
  //  let p1 = add_step(
  //    ls_dir,
  //    add_step(
  //      Belt.List.keep(_, is_file_exn),
  //      add_step(Belt.List.map(_, x => {x->lstat}.st_size), add_step(list_sum, Empty)),
  //    ),
  //  )

  let p1 = Step(
    ls_dir,
    Step(
      Belt.List.keep(_, is_file_exn),
      Step(Belt.List.map(_, x => {x->lstat}.st_size), Step(list_sum, Empty)),
    ),
  )

  Js.log("using pipeline GADT p1")
  exec(p1, ".")->Js.log
  exec(p1, "..")->Js.log

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
    (output, List.reverse(rev_profile))
  }

  Js.log("using pipeline GADT with profile p1")
  exec_with_profile(p1, ".")->Js.log
}

//
// Narrowing the Possibilities
//
// As this kind of complexity creeps in, it can be useful to be able to track the state of a given
// request at the type level, and to use that to narrow the set of states a given request can be
// in, thereby removing some extra case analysis and error handling, which can reduce the
// complexity of the code and remove opportunities for mistakes.
//
// One way of doing this is to mint different types to represent different states of the request,
// e.g., one type for an incomplete request where various fields are optional, and a different
// type where all of the data is mandatory.
//
// While this works, it can be awkward and verbose. With GADTs, we can track the state of the
// request in a type parameter, and have that parameter be used to narrow the set of available
// cases, without duplicating the type.

// A COMPLETION-SENSITIVE OPTION TYPE
//
module Narrowing_the_Possibilities = {
  // The definition of the types doesn’t really matter, since we’re never instantiating these types,
  // just using them as markers of different states. All that matters is that the types are distinct.
  type incomplete = Incomplete
  type complete = Complete

  // Now we can mint a completeness-sensitive option type. Note the two type variables: the first
  // indicates the type of the contents of the option, and the second indicates whether this is
  // being used in an incomplete state.
  type rec coption<_, _> =
    | Absent: coption<_, incomplete>
    | Present('a): coption<'a, _>

  let get = (Present(x): coption<_, complete>) => x

  module User_name = {
    type t
  }
  module User_id = {
    type t
  }
  module Permissions = {
    type t
    let check = (permissions, user_id) => (permissions, user_id)
  }

  // A COMPLETION-SENSITIVE REQUEST TYPE
  //
  // There’s a single type parameter for the logon_request that marks whether it’s complete,
  // at which point, both the user_id and permissions fields will be complete as well.
  type logon_request<'c> = {
    user_name: User_name.t,
    user_id: coption<User_id.t, 'c>,
    permissions: coption<Permissions.t, 'c>,
  }

  let set_user_id = (request, x) => {...request, user_id: Present(x)}
  let set_permissions = (request, x) => {...request, permissions: Present(x)}

  let check_completeness = (request): option<logon_request<complete>> => {
    switch (request.user_id, request.permissions) {
    | (Absent, _)
    | (_, Absent) =>
      None
    | (Present(_) as user_id, Present(_) as permissions) => Some({...request, user_id, permissions})
    }
  }

  let authorized = (request: logon_request<complete>) => {
    let {user_id: Present(user_id), permissions: Present(permissions), _} = request
    Permissions.check(permissions, user_id)
  }
}
