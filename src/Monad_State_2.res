//
// ref: http://blogs.perl.org/users/cyocum/2012/11/writing-state-monads-in-ocaml.html
//
let log = Js.log
let log2 = Js.log2

module type MONAD = {
  type t<'a>
  let bind: (t<'a>, 'a => t<'b>) => t<'b>
  let return: 'a => t<'a>
}

module type STATE = {
  type t
  let empty: t
}

module type STATE_MONAD = (State: STATE) =>
{
  include MONAD
  let access: t<'a> => 'a
  let put: State.t => t<unit>
  let get: t<State.t>
}

module StateMonad: STATE_MONAD = (State: STATE) => {
  type state = State.t
  type t<'a> = state => ('a, state)

  let bind = (m, f, s) => {
    switch m(s) {
    | (x, s') => f(x, s')
    }
  }

  let return = (a, s) => {
    (a, s)
  }

  let access = m => {
    switch m(State.empty) {
    | (x, s) => x
    }
  }

  let put = (s, _) => {
    ((), s)
  }

  let get = s => {
    (s, s)
  }
}

module IntStateMonad = StateMonad({
  type t = int
  let empty = 0
})

let _ = {
  let blah = IntStateMonad.return(1)
  let blah2 = IntStateMonad.bind(blah, i => IntStateMonad.return(i + 1))
  IntStateMonad.access(blah2)->log
}
