//
// ref: http://blogs.perl.org/users/cyocum/2012/11/writing-state-monads-in-ocaml.html
//
@@uncurried
@@uncurried.swap

let log = Js.log
let log2 = Js.log2

module type MONAD = {
  type t<'a>
  let bind: (. t<'a>, 'a => t<'b>) => t<'b>
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

  let bind: (. t<'a>, 'a => t<'b>) => t<'b> = (. m, f) => {
    s => {
      switch m(s) {
      | (x, s') => f(x)(s')
      }
    }
  }

  let return = a => {
    s => (a, s)
  }

  let access = m => {
    switch m(State.empty) {
    | (x, s) => x
    }
  }

  let put = s => {
    _ => {
      ((), s)
    }
  }

  let get = {
    s => {
      (s, s)
    }
  }
}

module IntStateMonad = StateMonad({
  type t = int
  let empty = 0
})

let _ = {
  open IntStateMonad
  let blah = return(1)
  let blah2 = blah->bind(x => return(succ(x)))
  access(blah2)->(log2(_, "test1"))
}
