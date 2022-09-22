//
// https://blog.shaynefletcher.org/2015/02/fold-left-via-fold-right.html
//
// Fold left via fold right
// The puzzle is to express fold_left entirely in terms of fold_right. For example, an attempted
// solution like
let log = Js.log
let log2 = Js.log2

let fold_right = List.fold_right

let fold_left_ = (f, e, s) => List.rev(fold_right((a, acc) => f(acc, a), e, List.rev(s)))

let fold_left = (f, e, s) => List.fold_right((a, acc, x) => acc(f(x, a)), s, x => x, e)

fold_left((acc, x) => list{x * x, ...acc}, list{}, list{1, 2, 3})->Belt.List.toArray->log
fold_right((x, acc) => list{x * x, ...acc}, list{1, 2, 3}, list{})->Belt.List.toArray->log
