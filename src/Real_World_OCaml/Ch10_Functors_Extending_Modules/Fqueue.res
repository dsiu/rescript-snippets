type t<'a> = (list<'a>, list<'a>)
let empty = (list{}, list{})

let enqueue = ((in_list, out_list), x) => {
  (list{x, ...in_list}, out_list)
}

let dequeue = ((in_list, out_list)) => {
  switch out_list {
  | list{hd, ...tl} => Some(hd, (in_list, tl))
  | list{} =>
    switch List.rev(in_list) {
    | list{} => None
    | list{hd, ...tl} => Some(hd, (list{}, tl))
    }
  }
}

let fold = ((in_list, out_list), ~init, ~f) => {
  let after_out = Belt.List.reduce(out_list, ~init, ~f)
  in_list->Belt.List.reverse->Belt.List.reduce(~init=after_out, ~f=(acc, x) => f(acc, x))
}
