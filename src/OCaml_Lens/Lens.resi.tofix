@@ocaml.text(/*
   Copyright (c) 2011-2012 Alessandro Strada

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

" Functional lenses.

    Based on F# implementation in {{:https://github.com/fsharp/fsharpx}FSharpx}
    (see {{:https://github.com/fsharp/fsharpx/blob/master/src/FSharpx.Core/Lens.fs}src/FSharpx.Core/Lens.fs} for the original implementation)

    @see <http://bugsquash.blogspot.com/2011/11/lenses-in-f.html> Lenses in F#
    @see <http://stackoverflow.com/questions/8179485/updating-nested-immutable-data-structures> Stackoverflow question about Updating nested immutable data structures
    @see <http://stackoverflow.com/questions/5767129/lenses-fclabels-data-accessor-which-library-for-structure-access-and-mutatio> Haskell libraries for structure access and mutation
    @see <http://www.youtube.com/watch?v=efv0SQNde5Q> Functional lenses for Scala by Edward Kmett on YouTube
    @see <http://patternsinfp.wordpress.com/2011/01/31/lenses-are-the-coalgebras-for-the-costate-comonad/> Lenses are the coalgebras for the costate comonad by Jeremy Gibbons
")

@ocaml.doc(" Lens type definition ")
type t<'a, 'b> = {
  @ocaml.doc(" Functional getter ")
  get: 'a => 'b,
  @ocaml.doc(" Functional setter ")
  set: ('b, 'a) => 'a,
}

@ocaml.doc(" Updates a value through a lens ")
let modify: (t<'a, 'b>, 'b => 'b, 'a) => 'a

// [dsiu]
let _get: ('a, t<'a, 'b>) => 'b
let _set: ('b, 'a, t<'a, 'b>) => 'a
let _modify: ('a => 'a, t<'b, 'a>, 'b) => 'b

@@ocaml.text(" {3 Combinators} ")

@ocaml.doc(" Sequentially composes two lenses ")
let compose: (t<'a, 'b>, t<'c, 'a>) => t<'c, 'b>

@ocaml.doc(" Pairs two lenses ")
let pair: (t<'a, 'b>, t<'c, 'd>) => t<('a, 'c), ('b, 'd)>

@ocaml.doc(" Pairs three lenses ")
let pair3: (t<'a, 'b>, t<'c, 'd>, t<'e, 'f>) => t<('a, 'c, 'e), ('b, 'd, 'f)>

@ocaml.doc(" Selects a lens checking a predicate.

    [cond pred lensTrue lensFalse]: [pred] is applied to source. If [true], [lensTrue] is selected. If [false], [lensFalse] is selected. ")
let cond: ('a => bool, t<'a, 'b>, t<'a, 'b>) => t<'a, 'b>

@@ocaml.text(" {3 State monad integration} ")

@ocaml.doc(" Gets a value from the state monad. ")
let get_state: (t<'a, 'b>, 'a) => ('b, 'a)

@ocaml.doc(" Puts a value in the state monad. ")
let put_state: (t<'a, 'b>, 'b, 'a) => (unit, 'a)

@ocaml.doc(" Modifies a value in the state monad. ")
let modify_state: (t<'a, 'b>, 'b => 'b, 'a) => (unit, 'a)

@@ocaml.text(" {3 Stock lenses} ")

@ocaml.doc(" Trivial lens ")
let ignore: t<'a, unit>

@ocaml.doc(" Identity lens ")
let id: t<'a, 'a>

@ocaml.doc(" Gets/sets the first element in a pair ")
let first: t<('a, 'b), 'a>

@ocaml.doc(" Gets/sets the second element in a pair ")
let second: t<('a, 'b), 'b>

@ocaml.doc(" Gets/sets the first element in a list ")
let head: t<list<'a>, 'a>

@ocaml.doc(" Gets/sets the tail of a list ")
let tail: t<list<'a>, list<'a>>

@ocaml.doc(" Lens for a particular key in a hashtable ")
let for_hash: 'a => t<Hashtbl.t<'a, 'b>, option<'b>>

@ocaml.doc(" Lens for a particular key in an associative list ")
let for_assoc: 'a => t<list<('a, 'b)>, option<'b>>

@ocaml.doc(" Lens for a particular position in an array ")
let for_array: int => t<array<'a>, 'a>

@ocaml.doc(" Lens for a particular position in a list ")
let for_list: int => t<list<'a>, 'a>

@ocaml.doc(" Lens for extracting the value from an option type (same as Option.get) ")
let option_get: t<option<'a>, 'a>

@@ocaml.text(" {3 List combinators} ")

@ocaml.doc(" Creates a lens that maps the given lens in a list ")
let list_map: t<'a, 'b> => t<list<'a>, list<'b>>

@@ocaml.text(" {3 Isomorphism} ")

@ocaml.doc(" Applies an isomorphism to the value viewed through a lens ")
let xmap: ('a => 'b, 'b => 'a, t<'c, 'a>) => t<'c, 'b>

@ocaml.doc(" Infix operators ")
module Infix: {
  // [dsiu]
  //  @ocaml.doc(" Get operator ")
  //  let \"|.": ('a, t<'a, 'b>) => 'b

  @ocaml.doc(" Set operator ")
  let \"^=": (t<'a, 'b>, 'b, 'a) => 'a

  @ocaml.doc(" Mod operator ")
  let \"^%=": (t<'a, 'b>, 'b => 'b, 'a) => 'a

  @@ocaml.text(" {3 Composition} ")

  @ocaml.doc(" Flipped compose operator ")
  let \"|--": (t<'a, 'b>, t<'b, 'c>) => t<'a, 'c>

  @ocaml.doc(" Compose operator ")
  let \"--|": (t<'a, 'b>, t<'c, 'a>) => t<'c, 'b>

  @ocaml.doc(" Pair operator ")
  let \"***": (t<'a, 'b>, t<'c, 'd>) => t<('a, 'c), ('b, 'd)>

  @@ocaml.text(" {3 Pseudo-imperatives} ")

  let \"+=": (t<'a, int>, int, 'a) => 'a

  let \"-=": (t<'a, int>, int, 'a) => 'a
}

@ocaml.doc(" Infix operators for the state monad ")
module StateInfix: {
  @ocaml.doc(" Set operator ")
  let \"^=!": (t<'a, 'b>, 'b, 'a) => (unit, 'a)

  @@ocaml.text(" {3 Pseudo-imperatives} ")

  let \"+=!": (t<'a, int>, int, 'a) => (unit, 'a)

  let \"-=!": (t<'a, int>, int, 'a) => (unit, 'a)

  let \"@=!": (t<'a, list<'b>>, list<'b>, 'a) => (unit, 'a)
}
