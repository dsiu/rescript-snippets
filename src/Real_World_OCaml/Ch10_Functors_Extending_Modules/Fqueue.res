//open Base

type t<'a> = (list<'a>, list<'a>)

let empty = (list{}, list{})

let enqueue = ((in_list, out_list), x) => (list{x, ...in_list}, out_list)

let dequeue = ((in_list, out_list)) =>
  switch out_list {
  | list{hd, ...tl} => Some(hd, (in_list, tl))
  | list{} =>
    switch List.rev(in_list) {
    | list{} => None
    | list{hd, ...tl} => Some(hd, (list{}, tl))
    }
  }

// ocaml 'acc 'a

let fold = ((in_list, out_list), ~init, ~f) => {
  let list_fold = (~init, ~f, xs) => Js.List.foldLeft((. acc, x) => f(acc, x), init, xs)
  let list_fold_right = (~init, ~f, xs) => Js.List.foldRight((. acc, x) => f(acc, x), xs, init)

  let after_out = list_fold(~init, ~f, out_list)

  list_fold_right(~init=after_out, ~f=(x, acc) => f(acc, x), in_list)
}
