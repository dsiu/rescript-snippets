module type S = {
  @ocaml.doc(" ['a t] is the type of sets whose elements are of type ['a]. ")
  type t<'a>

  @ocaml.doc(" [empty] is the empty set ")
  let empty: t<'a>

  @ocaml.doc(" [mem x s] is whether [x] is an element of [s]. ")
  let mem: ('a, t<'a>) => bool

  @ocaml.doc(" [add x s] is the set that contains [x] and all the elements of [s]. ")
  let add: ('a, t<'a>) => t<'a>

  @ocaml.doc(" [elements s] is a list containing the elements of [s].  No guarantee
      is made about the ordering of that list, but each is guaranteed to
      be unique. ")
  let elements: t<'a> => list<'a>
}
