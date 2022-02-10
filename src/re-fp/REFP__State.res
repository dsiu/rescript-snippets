module State = {
  // type t<'a, 'r> = 'r => 'a
  type t<'a, 'r, 'w> = 'r => ('a, 'w)

  let ask: unit => t<'a, 'r, 'w> = () => REFP__Functions.identity
  let asks: ('r => ('a, 'w)) => t<'a, 'r, 'w> = REFP__Functions.identity
  let from = (a): t<'a, 'r, 'w> => _ => a

  let map = (fa: t<'a, 'r, 'w>, f: 'a => 'b): t<'b, 'r, 'w> =>
    r => {
      let (a, w) = fa(r)
      (f(a), w)
    }

  //  let ap = (fa: t<'a, 'r>, fab: t<'a => 'b, 'r>): t<'b, 'r> => r => fab(r)(fa(r))

  let chain = (fa: t<'a, 'r, 'w>, f: 'a => t<'b, 'r, 'w>): t<'b, 'r, 'w> =>
    r => {
      let (a, w) = fa(r)
      f(a)(w)
    }
}

include State
