// https://www.craigfe.io/posts/tail-recursion-modulo-cons
//

// If the last action of a function f is to call another function g, the language run-time
// doesn't need to keep  f's stack frame around when calling g:

let log = Js.log

let g = n => n

let f = n => {
  log("Hello World")
  g(n + 1)
}

// Instead, the run-time may `re-purpose'  f's stack frame for g, saving space and time in stack
// (de)allocations. This optimisation, known as tail call elimination, is useful in many language
// paradigms. It is useful in functional programming languages for which recursion is the idiomatic
// way to repeat actions.

// Unfortunately, many standard uses of recursion are not tail-call optimisable:

let rec map = (f, l) => {
  switch l {
  | list{} => list{}
  | list{x, ...xs} => {
      let y = f(x)
      list{y, ...map(f, xs)}
    }
  }
}

// The x:ss case proceeds as follows:
// 1. Computer y:=f(x)
// 2. Recurively compute t1 := map f xs;
// 3. Allocate t2 := y::t1 on the heap
// 4. Return t2

// Step 3 prevents the tail-call optimisation: the runtime must build the list node after computing
// the tail with map f xs. Our map function is almost tail-recursive: if not for the data
// constructor (::), it would be. We call such functions 'tail recursive modulo cons'. There are two
// ways to make map fully tail-recursive:

// * We could build the result list in reverse order, then reverse it in one pass at the end. This
// is not ideal since our intermediate list requires time to build and creates work for the garbage
// collector.

// We could change the list type to allow us to build the list node first, and later fill in the
// correct tail. In OCaml, this needs a ref indirection.

// Let's try the latter approach. We introduce a ref and pass the 'tail to be filled in later' as an
// explicit argument:

//type rec mutable_list<'a> = list('a, ref<mutable_list<'a>>) | []

//type 'a mutable_list = (::) of 'a * 'a mutable_list ref | []
//
//let rec map f xs =
//    let rec inner res = function
//     | [] -> ()
//     | x :: xs ->
//        let y = f x in
//        (* create an 'incomplete' list node *)
//        let tail = ref [] in
//        res := y :: tail;
//        inner tail !xs (* tail call! *)
//    in
//    let res = ref [] in
//    inner res xs; !res
