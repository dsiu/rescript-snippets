//
// Dictionaries as functions
//
// https://blog.shaynefletcher.org/2016/04/dictionaries-as-functions.html
//
@@warning("-27")

// This is an "oldie but a goodie". It's super easy.
//
// A dictionary is a data structure that represents a map from keys to values.
// The question is, can this data structure and its characteristic operations be encoded using
// only functions?
//
// The answer of course is yes and indeed, here's one such an encoding in OCaml.
//

/* The type of a dictionary with keys of type ['a] and values of type
 ['b] */
type dict<'a, 'b> = 'a => option<'b>

/* The empty dictionary maps every key to [None] */
let empty = (k: 'a): option<'b> => None

/* [add d k v] is the dictionary [d] together with a binding of [k] to
 [v] */
let add = (d: dict<'a, 'b>, k: 'a, v: 'b): dict<'a, 'b> =>
  l =>
    if l == k {
      Some(v)
    } else {
      d(l)
    }

/* [find d k] retrieves the value bound to [k] */
let find = (d: dict<'a, 'b>, k: 'a): option<'b> => d(k)

/* e.g.

  Name                            | Age
  ================================+====
  "Felonius Gru"                  |  53
  "Dave the Minion"               | 4.5e3
  "Dr. Joseph Albert Nefario"     |  80

*/
let despicable = add(
  add(add(empty, "Felonius Gru", 53), "Dave the Minion", int_of_float(4.5e3)),
  "Dr. Nefario",
  80,
)

(find(despicable, "Dave the Minion") -> (
  x =>
    switch x {
    | Some(x) => x
    | _ => failwith("Not found")
    }
))->Js.log
