// https://github.com/Risto-Stevcev/bastet
// https://risto-stevcev.github.io/bastet/bastet/index.html

@@uncurried
@@uncurried.swap

open BsBastet

let log = Js.log
let log2 = Js.log2

module T = Functors.ListF.Option.Traversable
list{Some("foo"), Some("bar")}->T.sequence->log
list{1, 2, 3, 4, 5}->Functors.ListF.Int.Show.show->log

// Use kliesli composition
let \">=>" = Option.Infix.\">=>"

type form = {name: string, address: option<string>}

let get_form = () => Some({name: "Foo", address: Some("123 Bar St.")})

let get_address = form => form.address

let get_form_address = get_form->(\">=>"(get_address, ...))

get_form_address()->log

// Instantiated Functors
let fmap_add = Functors.ArrayF.Int.Additive.Fold_Map.fold_map
[1, 2, 3, 4, 5]->fmap_add(Functions.id, _)->log

// Don't Overuse Infix
// Don't overuse infix operators. If the code is combinatorial it can make it more readable, but in
// a lot of cases the prefix operators are simpler and easier to read. If you do use infix
// operators, prefer local opens over global opens to avoid polluting the toplevel:

let trim_all = strings => {
  open List.Infix
  (s => StringLabels.trim(s))->\"<$>"(strings)
}
trim_all(list{"foo   ", "bar", "    baz"})->log

// Use Abbreviated Modules
//
//Abbreviated modules can make code both terser and easier to read in some situations, like for example where two different semigroups are used in the same function and infix operators can't be used:

type game = {score: int, disqualified: bool}

let total_score = (a, b) => {
  module I = Int.Additive.Semigroup
  module B = Bool.Disjunctive.Semigroup
  {score: I.append(a.score, b.score), disqualified: B.append(a.disqualified, b.disqualified)}
}

let result = {
  let game_1 = {score: 4, disqualified: false}
  let game_2 = {score: 2, disqualified: true}

  total_score(game_1, game_2)
}
result->log
