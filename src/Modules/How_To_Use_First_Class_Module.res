// https://forum.rescript-lang.org/t/how-to-use-the-first-class-module-in-rescript/3238/6

// Normally, modules (module M = ...) and values (let x = ...) live on different “layers” of the
// language. First-class modules allow you to convert a module to a value (i.e. make it “first
// class”) and pass it around like any other value.
//
//Suppose you have a module that looks like this:

module Int = {
  type t = int
  let cmp = (a: t, b: t) => compare(a, b)
}

// FCMs need to be annotated with a type. Once we define a module type, we can use module() to turn
// our Int module into a FCM.

module type Comparable = {
  type t
  let cmp: (t, t) => int
}
let int_fcm: module(Comparable with type t = int) = module(Int)

// You can also alias a module type to make the annotations more readable:
type comparable<'a> = module(Comparable with type t = 'a)
let int_fcm: comparable<int> = module(Int)

// We can “unpack” a FCM and use it as a regular module again:
module Int2 = unpack(int_fcm)

// It gets a bit trickier if you want to use the FCM in a function. We have to use “locally abstract
// types,” which basically just means defining a new type as if it was a function argument.
let f = (type t, a, b, m: comparable<t>) => {
  module M = unpack(m)
  M.cmp(a, b)
}

// Why would you use a first-class module? Most of the time, you’re better off just using a record.
// FCMs compile to JavaScript objects just like records do, but their typing rules are much more
// complicated. You only really need to use FCMs if you’re creating new types dynamically. (This is
// the reason why the Belt library uses FCMs.) Unless you’re a library author, or you’re relying on
// some very advanced type tricks, then it’s unlikely you’ll need to deal with FCMs a lot. Most of
// the type, you’ll just have to use module() to create one and pass it to a library’s function.
