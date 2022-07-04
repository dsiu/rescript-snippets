module type S = {
  @ocaml.doc(" [('k, 'v) t] is the type of maps that bind keys of type ['k] to
      values of type ['v]. ")
  type t<'k, 'v>

  @ocaml.doc(" [empty] does not bind any keys. ")
  let empty: t<'k, 'v>

  @ocaml.doc(" [insert k v m] is the map that binds [k] to [v], and also contains
      all the bindings of [m].  If [k] was already bound in [m], that old
      binding is superseded by the binding to [v] in the returned map. ")
  let insert: ('k, 'v, t<'k, 'v>) => t<'k, 'v>

  @ocaml.doc(" [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. ")
  let lookup: ('k, t<'k, 'v>) => 'v

  @ocaml.doc(" [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. ")
  let bindings: t<'k, 'v> => list<('k, 'v)>
}
