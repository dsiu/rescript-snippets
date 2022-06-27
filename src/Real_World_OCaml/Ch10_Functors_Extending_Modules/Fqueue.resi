type t<'a>
let empty: t<'a>
let enqueue: (t<'a>, 'a) => t<'a>
let dequeue: t<'a> => option<('a, t<'a>)>
let fold: (t<'a>, ~init: 'acc, ~f: ('acc, 'a) => 'acc) => 'acc
