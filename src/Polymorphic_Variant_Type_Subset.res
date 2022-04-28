//
// https://forum.rescript-lang.org/t/void-never-type-in-rescript/3246/6?u=dsiu
//

// If you want to constrain a type to a subset, you should use polymorphic variants in ReScript:

type t<'a> = [#BusinessErr('a) | #TechErr]

let f = (x: [#TechErr]) =>
  switch x {
  | #TechErr => "hello"
  }
f(#TechErr)->Js.log
#BusinessErr("no way")->Js.log
// f(#BusinessErr("no way"))->Js.log // should not compile

// Have a look at Polymorphic Variant | ReScript Language Manual
// https://rescript-lang.org/docs/manual/latest/polymorphic-variant#extra-constraints-on-types
