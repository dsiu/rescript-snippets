module M: OP_Map.S = {
  @ocaml.doc(" The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      No duplicate keys may occur. ")
  type t<'k, 'v> = list<('k, 'v)>
  let empty = list{}
  let insert = (k, v, m) => list{(k, v), ...List.remove_assoc(k, m)}
  let lookup = (k, m) => List.assoc(k, m)
  let bindings = m => m
}
