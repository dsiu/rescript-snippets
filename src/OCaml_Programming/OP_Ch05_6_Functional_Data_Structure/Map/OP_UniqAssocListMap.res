module M: OP_Map.S = {
  @ocaml.doc(" The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      No duplicate keys may occur. ")
  type t<'k, 'v> = list<('k, 'v)>
  let empty = list{}
  let insert = (k, v, m) => list{(k, v), ...List.removeAssoc(m, k, (a, b) => a == b)}
  let lookup = (k, m) => List.getAssoc(m, k, (a, b) => a == b)->Option.getExn
  let bindings = m => m
}
