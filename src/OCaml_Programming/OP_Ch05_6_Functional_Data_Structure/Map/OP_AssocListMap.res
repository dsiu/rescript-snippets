module M: OP_Map.S = {
  @ocaml.doc(" The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      If a key appears more than once in the list, it is bound to the
      the left-most occurrence in the list. ")
  type t<'k, 'v> = list<('k, 'v)>
  let empty = list{}
  let insert = (k, v, m) => list{(k, v), ...m}
  let lookup = (k, m) => List.assoc(k, m)
  let keys = m => {
    open List
    m |> map(fst) |> sort_uniq(Pervasives.compare)
  }
  let bindings = m => m |> keys |> List.map(k => (k, lookup(k, m)))
}
