open Belt
let log = Js.Console.log

// map string
let dump_mapString_of = (f, m) =>
  Map.String.forEach(m, (k, v) => {
    log(`key:${k}, val:${v->f}`)
  })

let dump_mapString_of_int = dump_mapString_of(Int.toString)

// map int
let dump_mapInt_of = (f, m) =>
  Map.Int.forEach(m, (k, v) => {
    log(`key:${k->Int.toString}, val:${f(v)}`)
  })
let dump_mapInt_of_int = dump_mapInt_of(Int.toString)

// mutable map int
let dump_mutableMapInt_of = (f, m) =>
  MutableMap.Int.forEach(m, (k, v) => {
    log(`key:${k->Int.toString}, val:${v->f}`)
  })
let dump_mutableMapInt_of_int = dump_mutableMapInt_of(Int.toString)

// list
let dump_list = List.forEach(_, log)

// array
let flattenArray = (arr: array<array<'a>>): array<'a> =>
  arr->Array.reduce([], (acc, x) => acc->Array.concat(x))
