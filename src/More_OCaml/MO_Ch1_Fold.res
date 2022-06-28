// Chapter 1
// Unravelling "Fold"
module List = Js.List

// utils
let log = Js.log
let logList = l => l->List.toVector->log
let log2 = (x, y) => Js.log2(y, x)
let logList2 = (l, str) => l->List.toVector->log2(str)

let rec fold_left = (f, a, l) =>
  switch l {
  | list{} => a
  | list{h, ...t} => fold_left(f, f(a, h), t)
  }

list{1, 2, 3}->fold_left(\"+", 0, _)->log
// fold_left("+", 0, {1,2,3})
// fold_left("+", 1, {2,3})
// fold_left("+", 3, {3})
// fold_left("+", 6, {})
// 6

// Note also that fold_right is not
// tail-recursive (the intermediate expression it builds is proportional to the size of the input)

let rec fold_right = (f, l, a) => {
  switch l {
  | list{} => a
  | list{h, ...t} => f(h, fold_right(f, t, a))
  }
}

list{1, 2, 3}->fold_right(\"+", _, 0)->log
// fold_right("+", {1,2,3}, 0)
// "+" ( 1, fold_right("+", {2,3}, 0) )
// "+" ( 1, "+" ( 2, fold_right("+", {3}, 0) ) )
// "+" ( 1, "+" ( 2, "+" ( 3, fold_right("+", {}, 0 ) ) ) )
// "+" ( 1, "+" ( 2, "+" ( 3, 0 ) ) ) )
// "+" ( 1, "+" ( 2, 3 ) )
// "+" ( 1, 5 )
// 6

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

let copy_l = l => {
  fold_left((e, a) => list{a, ...e}, list{}, List.rev(l))
}
list{2, 5, 6}->copy_l->logList2("copy_l")

let append = (x, y) => {
  fold_right((e, a) => list{e, ...a}, x, y)
}

append(list{8, 1, 2}, list{9, 6, 3})->logList2("append")

let split = l => {
  fold_right(((x, y), (xs, ys)) => (list{x, ...xs}, list{y, ...ys}), l, (list{}, list{}))
}

split(list{(1, "one"), (2, "two")})->log
//split(list{(1, "one"), (2, "two")})->logList2("split")

//
// Folding over trees
//
type rec tree<'a> =
  | Lf
  | Br('a, tree<'a>, tree<'a>)

let rec fold_tree = (f, e, t) => {
  switch t {
  | Lf => e
  | Br(x, l, r) => f(x, fold_tree(f, e, l), fold_tree(f, e, r))
  }
}

let tree_size = t => fold_tree((_, l, r) => {1 + l + r}, 0, t)
let tree_sum = t => fold_tree((x, l, r) => x + l + r, 0, t)

let exp_tr = Br(1, Br(0, Lf, Lf), Br(6, Br(4, Lf, Lf), Lf))

let tree_preorder = t => fold_tree((x, l, r) => {list{x}->append(l)->append(r)}, list{}, t)
let tree_inorder = t => fold_tree((x, l, r) => l->append(list{x})->append(r), list{}, t)
let tree_postorder = t => fold_tree((x, l, r) => l->append(r)->append(list{x}), list{}, t)

exp_tr->tree_preorder->logList2("preorder")
exp_tr->tree_inorder->logList2("inorder")
exp_tr->tree_postorder->logList2("postorder")

//
// Ch1 Quetions
//

// 1. Write a function which, given a list of integers representing expenses, removes them from a
// budget, again represented by an integer.
let q1_deduct = (exp, budget) => {
  fold_left(\"-", budget, exp)
}
q1_deduct(list{1, 2, 3}, 10)->log2("q1")

// 2. Calculate the length of a list using one of the fold_ functions.
let q2_length = l => fold_right((_, a) => a + 1, l, 0)
list{4, 6, 1, 6, 4, 9, 2, 1}->q2_length->log2("q2")

// 3. Use one of the fold_ functions to find the last element of list, if any. Behave sensibly if the list is empty.”
let q3_last = l => {
  switch l {
  | list{} => None
  | list{a} => Some(a)
  | list{h, ...t} => Some(fold_left((_, e) => e, h, t))
  }
}
list{4, 6, 1, 6, 4, 9, 2}->q3_last->log2("q3")

// 4. Write a function to reverse a list, using one of the fold_ functions
let q4_rev = l => fold_left((e, a) => list{a, ...e}, list{}, l)
list{1, 2, 3, 4, 5}->q4_rev->logList2("q4")

// 5. Write a version of List.mem using one of the fold_ functions. Now setify can be defined
// entirely using folds.
// let q5_member = (x,l)
