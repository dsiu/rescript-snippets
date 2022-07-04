module type S = {
  type t<'a>
  exception Empty
  let empty: t<'a>
  let is_empty: t<'a> => bool
  let push: ('a, t<'a>) => t<'a>
  let peek: t<'a> => 'a
  let pop: t<'a> => t<'a>
  let size: t<'a> => int
  let to_list: t<'a> => list<'a>
}
