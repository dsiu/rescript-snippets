// Chapter 1
// Unravelling "Fold"
module List = Js.List

let log = Js.log
let logList = l => l->List.toVector->log
let log2 = Js.log2
let logList2 = (l, str) => l->List.toVector->log2(str, _)

let rec fold_left = (f, a, l) =>
  switch l {
  | list{} => a
  | list{h, ...t} => fold_left(f, f(a, h), t)
  }

let rec fold_right = (f, l, a) => {
  switch l {
  | list{} => a
  | list{h, ...t} => f(h, fold_right(f, t, a))
  }
}

let plus = (a, b) => a + b
list{1, 2, 3}->fold_left(plus, 0, _)->log

let max = (a, b) => a > b ? a : b
list{2, 4, 6, 20, 1}->fold_left(Js.Math.max_int, Js.Int.min, _)->log

let all = l => fold_left(\"&&", true, l)
let any = l => fold_left(\"||", false, l)

let map = (f, l) => {
  fold_right((e, a) => list{f(e), ...a}, l, list{})
}

list{2, 9, 1}->map(x => x * 2, _)->logList2("map")

// Who would have thought that fold_right was the more fundamental function? At the cost of a list
// reversal, we can make fold_right tail-recursive by defining it in terms of fold_left:
let fold_right_tr = (f, l, e) => {
  fold_left((x, y) => f(y, x), e, List.rev(l))
}

let copy = l => {
  fold_right((e, a) => list{e, ...a}, l, list{})
}
list{2, 5, 6}->copy->logList2("copy")

let append = (x, y) => {
  fold_right((e, a) => list{e, ...a}, x, y)
}

append(list{8, 1, 2}, list{9, 6, 3})->logList2("append")

let split = l => {
  fold_right(((x, y), (xs, ys)) => (list{x, ...xs}, list{y, ...ys}), l, (list{}, list{}))
}

split(list{(1, "one"), (2, "two")})->log
//split(list{(1, "one"), (2, "two")})->logList2("split")
