open Belt

// flatMap for Array
let dup = x => [x, x]
let src = [2, 4, 6]
let flatMap = (m, f) => {
  m->Array.reduce([], (a, x) => {a->Array.concat(x->f)})
}

src->Array.map(dup)->Js.log
src->flatMap(dup)->Js.log

// flatMap for List
let dupl = x => list{x, x}
let srcl = list{2, 4, 6}

let flatMapL = (m, f) => {
  m->List.reduce(list{}, (a, x) => {a->List.concat(x->f)})
}

srcl->List.map(dupl)->List.toArray->Js.log
srcl->flatMapL(dupl)->List.toArray->Js.log
