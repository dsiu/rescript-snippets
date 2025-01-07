module M: OP_Map.S = {
  @ocaml.doc(" The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      If a key appears more than once in the list, it is bound to the
      the left-most occurrence in the list. ")
  type t<'k, 'v> = list<('k, 'v)>
  let empty = list{}
  let insert = (k, v, m) => list{(k, v), ...m}
  let lookup = (k, m) => List.getAssoc(m, k, (a, b) => a == b)->Option.getExn
  let keys = m => {
    open List
    List.sort(List.map(m, fst), (a, b) => Pervasives.compare(a, b)->Ordering.fromInt)
  }
  let bindings = m => List.map(keys(m), k => (k, lookup(k, m)))
}
