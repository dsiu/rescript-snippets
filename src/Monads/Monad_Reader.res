//
// Creating a Reader Monad in ReScript
// ref: https://kevanstannard.github.io/rescript-blog/reader-monad.html
//

// Reader Type

// A Reader is a function that takes an "environment" variable, that can be any data, and performs some operation on it.
@@uncurried
@@uncurried.swap

type t<'e, 'a> = Reader('e => 'a)

// eg:
// let r = Reader(e => e + 1)

//
// run()
// The run() function takes a reader and an environment passes the environment to the reader function.
type run<'e, 'a> = (t<'e, 'a>, 'e) => 'a
let run: run<'e, 'a> = (Reader(r), env) => r(env)

// eg:
// let r1 = Reader(e => e + 1)
// let _ = run(r1, 1)
// -> 2

//
// return()
// The return() function creates a reader that always returns the same value, ignoring the environment.
type return<'e, 'a> = 'a => t<'e, 'a>
let return: return<'e, 'a> = a => Reader(_env => a)

// Example:
// let r2 = return(99)
// let _ = run(r2, 1)
// -> 99

//
// ask()
// The ask() function creates a reader that returns the environment variable.
type ask<'e> = unit => t<'e, 'e>
let ask: ask<'e> = () => Reader(env => env)

// Example:
// let r3 = ask()
// let _ = run(r3, 123)
// ->  123

// local()
// The local() function creates a reader that allows you to transform the environment value before passing it to a subsequent reader.
type local<'e, 'a> = ('e => 'e, t<'e, 'a>) => t<'e, 'a>
let local: local<'e, 'a> = (f, m) => Reader(env => run(m, f(env)))

// Example:
// let r4 = Reader(e => e + 1)
// let r5 = local(e => -e, r4)
// let _ = run(r5, 1)
// -> 0

//
// map()
// The map() function creates a reader that applies a function to the result provided by the reader.
type map<'e, 'a, 'b> = ('a => 'b, t<'e, 'a>) => t<'e, 'b>
let map: map<'e, 'a, 'b> = (f, m) => Reader(env => f(run(m, env)))

// Example:
// let r6 = Reader(e => e + 1)
// let r7 = map(x => x * 10, r6)
// let _ = run(r7, 1)
// -> 20

//
// bind()
// The bind() function is similar to map, except that the applied function must return a reader rather than a value. This allows two readers to be bound together.
type bind<'e, 'a, 'b> = ('a => t<'e, 'b>, t<'e, 'a>) => t<'e, 'b>
let bind: bind<'e, 'a, 'b> = (f, m) => Reader(env => run(f(run(m, env)), env))

// Example:
// let r8 = Reader(e => e + 1)
// let r9 = bind(x => Reader(_ => x * 2), r8)
// let _ = run(r9, 1)
// -> 4
// For this example, we could also use the return() function:
//
// let r10 = Reader(e => e + 1)
// let r11 = bind(x => return(x * 2), r10)
// let _ = run(r11, 1)
// -> 4
// And a more sophisitcated example:
//
// let greet = (name, greeting) => greeting ++ ": " ++ name
// let lines = Array.map(Js.log)
// let ra = Reader(greet("One"))
// let rb = Reader(greet("Two"))
// let rc = Reader(greet("Three"))
//
// let r12 = bind(a => bind(b => bind(c => return(lines([a, b, c])), rc), rb), ra)
// let _ = run(r12, "Hello")
// -> Hello: One
// -> Hello: Two
// -> Hello: Three

//
// bindFlip()
// An example which flips the the bind arguments
type bindFlip<'a, 'e, 'b> = (t<'e, 'a>, 'a => t<'e, 'b>) => t<'e, 'b>
let bindFlip: bindFlip<'a, 'e, 'b> = (m, f) => bind(f, m)

// Example:
// let greet = (name, greeting) => greeting ++ ": " ++ name
// let lines = Array.map(Js.log)
// let ra = Reader(greet("One"))
// let rb = Reader(greet("Two"))
// let rc = Reader(greet("Three"))
//
// let r13 = bindFlip(ra, a => bindFlip(rb, b => bindFlip(rc, c => return(lines([a, b, c])))))
// let _ = run(r13, "Goodbye")
// -> Goodbye: One
// -> Goodbye: Two
// -> Goodbye: Three

//
// References
// Reader monad in reasonml
// https://sketch.sh/s/NUtDN1ArEiJ1FIEfG6ZRoj/
//
// A simple reader monad example
// https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html
//
// Rationale Reader
// https://github.com/jonlaing/rationale/blob/9e206959b7d4de5ed96ef90ce71a268ccf624124/src/Reader.re
//
// A Layman's Guide to Functors in ReasonML
// https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
//
// reader.hs
// https://gist.github.com/egonSchiele/5752172
//
// Simple Reader monad in OCaml
// https://gist.github.com/VincentCordobes/fff2356972a88756bd985e86cce03023

// Reader monad in reasonml
// https://sketch.sh/s/NUtDN1ArEiJ1FIEfG6ZRoj/

//type env = {
//  name: string,
//  age: int,
//}
//
//let getName = env => env.name
//
//let to_string = (age, name) => "Name: " ++ name ++ ", Age: " ++ string_of_int(age)
//
//let _ = {
//  let _env = {name: "John", age: 30}
//  Js.Console.timeStart("reader")
//  let r =
//    return(24)
//    ->map(x => x + 1, _)
//    ->bind(x => ask()->map(getName, _)->map(to_string(x), _), _)
//    ->local(env => {...env, name: "Vicent"}, _)
//
//  let env = {name: "Jack", age: 85}
//  run(r, env)->Js.log
//  Js.Console.timeEnd("reader")
//   -> Name: Vicent, Age: 25
//}

type env2 = {state: int}

let _ = {
  let _getState = env2 => env2.state

  let r2 = return("danny")->(bind(x => {
      ask()->map(_getState, _)->(map(y => string_of_int(y) ++ x, _))
    }, _))

  let env2 = {state: 1}

  run(r2, env2)->Js.log
  run(r2, {state: 3})->Js.log
}
