type t<'a>

let empty: t<'a>

//@ocaml.doc(" [enqueue q el] adds [el] to the back of [q] ")
let enqueue: (t<'a>, 'a) => t<'a>

//@ocaml.doc(" [dequeue q] returns None if the [q] is empty, otherwise returns
//    the first element of the queue and the remainder of the queue ")
let dequeue: t<'a> => option<('a, t<'a>)>

//@ocaml.doc(" Folds over the queue, from front to back ")
let fold: (t<'a>, ~init: 'acc, ~f: ('acc, 'a) => 'acc) => 'acc
