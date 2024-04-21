// https://cryptologie.net/article/578/simple-introduction-to-monads-in-ocaml/
// Simple introduction to monads in OCaml
@@uncurried
@@uncurried.swap

let log = Js.log
let log2 = Js.log2

module type Monads = {
  type t<'a>
  let return: 'a => t<'a>
  let bind: (t<'a>, 'a => t<'b>) => t<'b>
}

//type option<'a> = None | Some('a)

// Sometimes, we want to chain operations on an option. That is, we want to operate on the value it
// might contain, or do nothing if it doesn't contain a value. For example:
let _ = {
  let x = Some(5)
  let y = None
  switch x {
  | None => None
  | Some(v1) =>
    switch y {
    | None => None
    | Some(v2) => Some(v1 + v2)
    }
  }
}

// Writing these nested match statements can be really tedious, especially the more there are, so
// there's a bind function in OCaml to simplify this:

let _ = {
  open Belt
  let x = Some(5)
  let y = None
  Option.flatMap(x, v1 => Option.flatMap(y, v2 => Some(v1 + v2)))
}

// https://www.cryptologie.net/article/581/state-monads-in-ocaml/
//
// State monads in OCaml
//
module CounterStateMonad = {
  type state = {next: int}
  type t<'a> = state => ('a, state)

  let return = a => {
    state => (a, state)
  }

  let bind: (t<'a>, 'a => t<'b>) => t<'b> = (t, f) => {
    state => {
      let (a, transient_state) = t(state)
      let (b, final_state) = f(a)(transient_state)
      (b, final_state)
    }
  }

  let new_var = (. _, state) => {
    let var = state.next
    let state = {next: state.next + 1}
    (var, state)
  }

  let negate = (. var, state) => {
    (0 - var, state)
  }

  let add = (. var1, var2, state) => {
    (var1 + var2, state)
  }

  //  Now we write things in an imperative way, without monads. Notice that we pass the state and
  //  return the state all the time, which can be tedious.
  let _ = {
    let run = state => {
      state->log2("start")
      let (a, state) = new_var((), state)
      (a, state)->log2("a")
      let (b, state) = negate(a, state)
      (b, state)->log2("b")
      let (c, state) = add(a, b, state)
      (c, state)->log2("c")

      (c, state)
    }

    let init_state = {next: 10}
    let (c, _) = run(init_state)
    c->log2("c final imperative") // should be always 0
  }

  // We can write the same with our monad type [t]:

  let _ = {
    let run = {
      bind(new_var((), _), a => {
        bind(negate(a, _), b => {
          bind(
            add(a, b, _),
            c => {
              return(c)
            },
          )
        })
      })
    }

    let init_state = {next: 10}
    let (c, _) = run(init_state)
    c->log2("c final bind") // sh
  }
}
