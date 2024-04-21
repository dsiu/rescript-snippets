// https://cs3110.github.io/textbook/chapters/modules/functors.html
//
@@uncurried
@@uncurried.swap

let log = Js.log
let log2 = Js.log2

module type X = {
  let x: int
}

// simple functor
//
module IncX = (M: X) => {
  let x = M.x + 1
}

module A = {
  let x = 0
}

module B = IncX(A)
module C = IncX(B)
B.x->(log2("B.x", _))
C.x->(log2("C.x", _))

module AddX = (M: X) => {
  let add = y => M.x + y
}

module Add42 = AddX({
  let x = 42
})

Add42.add(1)->(log2("Add42.add(1)", _))
// Note that the input module to AddX contains a value named x, but the output module from
// AddX does not

// It’s tempting to think that a functor is the same as extends in Java, and that the functor
// therefore extends the input module with new definitions while keeping the old definitions
// around too. The example above shows that is not the case. A functor is essentially just a
// function, and that function can return whatever the programmer wants. In fact the output of
// the functor could be arbitrarily different than the input.

//
// 5.9.1. Functor Syntax and Semantics
// https://cs3110.github.io/textbook/chapters/modules/functors.html#functor-syntax-and-semantics
//

// And functors can be parameterized on multiple structures:
// module XY = (M1: S1, M2: S2) => {
//  let z = M1.x + M2.y + 10
//}

// If you want to specify the output type of a functor, the syntax is again similar to functions:
//  module type Z = {
//    let z: int
//  }
//  module Z = (M1: S1, M2: S2): Z => {
//    let z = M1.x + M2.y + 10
//  }

//
// 5.9.2. Functor Type Syntax and Semantics
//
// module_type -> module_type
//

// For example, X -> Add below is a functor type, and it works for the AddX module we defined
// earlier in this section:
module type Add = {
  let add: int => int
}

module CheckAddX: X => Add = AddX

// Functor type syntax becomes more complicated if the output module type is dependent upon the
// input module type. For example, suppose we wanted to create a functor that pairs up a value from
// one module with another value:
module type T = {
  type t
  let x: t
}

module Pair1 = (M: T) => {
  let p = (M.x, 1)
}

// so we could also write:
// this is a functor type
module type P1 = (M: T) =>
{
  let p: (M.t, int)
}
// functor with the above type
module Pair1_: P1 = (M: T) => {
  let p = (M.x, 1)
}

// Inside the signature, the name M is in scope. That’s why we can write M.t in it, thereby ensuring
// that the type of the first component of pair p is the type from the specific module M that is
// passed into Pair1, not any other module. For example, here are two different instantiations:
module P0 = Pair1_({
  type t = int
  let x = 0
})
module PA = Pair1_({
  type t = char
  let x = 'a'
})

// NOTE:
// Functor types are an example of an advanced programming language feature called dependent types,
// with which the type of an output is determined by the value of an input. That’s different than
// the normal case of a function, where it’s the output value that’s determined by the input value,
// and the output type is independent of the input value.
//
// Dependent types enable type systems to express much more about the correctness of a program,
// but type checking and inference for dependent types is much more challenging. Practical
// dependent type systems are an active area of research. Perhaps someday they will become popular
// in mainstream languages.

// The module type of a functor’s actual argument need not be identical to the formal declared
// module type of the argument; it’s fine to be a subtype. For example, it’s fine to apply F below
// to either X or Z. The extra item in Z won’t cause any difficulty.
module F = (
  M: {
    let x: int
  },
) => {
  let y = M.x
}
module X = {
  let x = 0
}
module Z = {
  let x = 0
  let z = 0
}
module FX = F(X)
module FZ = F(Z)

// 5.9.3. The Map Module
//

// The Map module defines a functor Make that creates a structure implementing a map over a
// particular type of keys. That type is the input structure to Make. The type of that input
// structure is Map.OrderedType, which are types that support a compare operation:

module type OrderedType = {
  type t
  let compare: (t, t) => int
  //  The compare function’s specification is the same as that for the comparison argument to
  //  List.sort_uniq, which we previously discussed:
  //
  // The comparison should return 0 if two keys are equal.
  // The comparison should return a strictly negative number if the first key is lesser than the second.
  // The comparison should return a strictly positive number if the first key is greater than
  // the second.
}

// The output of Map.Make supports all the usual operations we would expect from a dictionary:
module type S = {
  type key
  type t<'a>
  let empty: t<'a>
  let mem: (key, t<'a>) => bool
  let add: (key, t<'a>) => t<'a>
  let find: (key, t<'a>) => 'a
}
// The type variable 'a is the type of values in the map. So any particular map module created by
// Map.Make can handle only one type of key, but is not restricted to any particular type of value.

// 5.9.3.1. An Example Map
//
// Here’s an example of using the Map.Make functor:

module IntHash = Belt.Id.MakeHashableU({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

Belt.HashMap.set(hMap, 0, "a")

// 5.9.3.2. Maps with Custom Key Types
// When the type of a key becomes complicated, we might want to write our own custom comparison
// function. For example, suppose we want a map in which keys are records representing names, and in
// which names are sorted alphabetically by last name then by first name. In the code below, we
// provide a module Name that can compare records that way:

type name = {first: string, last: string}

module Name = Belt.Id.MakeComparableU({
  type t = name
  let cmp = ({first: first1, last: last1}, {first: first2, last: last2}) => {
    switch compare(last1, last2) {
    | 0 => compare(first1, first2)
    | c => c
    }
  }
})
let empty = Belt.Map.make(~id=module(Name))
let nm = empty->Belt.Map.set({first: "danny", last: "siu"}, 1970)

// 5.9.3.3. How Map Uses Module Type Constraints
//
// In the standard library’s map.mli interface, the specification for Map.Make is:
//
// module Make (Ord : OrderedType) : S with type key = Ord.t
//
// The with constraint there is crucial. Recall that type constraints specialize a module type.
// Here, S with type key = Ord.t specializes S to expose the equality of S.key and Ord.t. In other
// words, the type of keys is the ordered type.

// This kind of use case is why module type constraints are quite important in effective programming
// with the OCaml module system. Often it is necessary to specialize the output type of a functor to
// show a relationship between a type in it and a type in one of the functor’s inputs. Thinking
// through exactly what constraint is necessary can be challenging, though!

// 5.9.4. Using Functors
//
//
exception Empty

module type Stack = {
  type t<'a>
  let empty: t<'a>
  let push: (. 'a, t<'a>) => t<'a>
  let peek: t<'a> => 'a
  let pop: t<'a> => t<'a>
}

module ListStack = {
  type t<'a> = list<'a>
  let empty = list{}
  let push = List.cons
  let peek = s => {
    switch s {
    | list{} => raise(Empty)
    | list{x, ..._} => x
    }
  }
  let pop = s => {
    switch s {
    | list{} => raise(Empty)
    | list{_, ...s} => s
    }
  }
}

module VariantStack = {
  type rec t<'a> =
    | E
    | S('a, t<'a>)
  let empty = E
  let push = (. x, s) => S(x, s)
  let peek = s => {
    switch s {
    | E => raise(Empty)
    | S(x, _) => x
    }
  }
  let pop = s => {
    switch s {
    | E => raise(Empty)
    | S(_, s) => s
    }
  }
}

{
  open ListStack
  (empty->push(1, _)->peek === 1)->log
}
// Unfortunately, to test a VariantStack, we’d have to duplicate that code:
{
  open VariantStack
  (empty->push(1, _)->peek === 1)->log
}

// And if we had other stack implementations, we’d have to duplicate the test for them, too. That’s
// not so horrible to contemplate if it’s just one test case for a couple implementations, but if
// it’s hundreds of tests for even a couple implementations, that’s just too much duplication to be
// good software engineering.

// Functors offer a better solution. We can write a functor that is parameterized on the stack
// implementation, and produces the test for that implementation:

module StackTester = (S: Stack) => {
  open S
  let tests = empty->push(1, _)->peek === 1
}
module ListStackTester = StackTester(ListStack)
module VariantStackTester = StackTester(VariantStack)

// Now whenever we invent a new test we add it to StackTester, and it automatically gets run on both
// stack implementations. Nice!

// There is still some objectionable code duplication, though, in that we have to write two lines of
// code per implementation. We can eliminate that duplication through the use of first-class
// modules:

let stacks = list{module(ListStack: Stack), module(VariantStack)}

let tests = m => {
  module S = unpack(m: Stack)
  module T = StackTester(S)
  T.tests
}

List.map(tests, stacks)->log
// Now it suffices just to add the newest stack implementation to the stacks list. Nicer!

//
// 5.9.4.2. Extending Multiple Modules
//
// Earlier, we tried to add a function of_list to both ListSet and UniqListSet without having any
// duplicated code, but we didn’t totally succeed. Now let’s really do it right.
//
// The problem we had earlier was that we needed to parameterize the implementation of of_list on
// the add function and empty value in the set module. We can accomplish that parameterization with
// a functor:

module type Set = {
  type t<'a>
  let empty: t<'a>
  let mem: (. 'a, t<'a>) => bool
  let add: (. 'a, t<'a>) => t<'a>
  let elements: t<'a> => list<'a>
}

module SetOfList = (S: Set) => {
  let of_list = lst => {
    List.fold_right(S.add, lst, S.empty)
  }
}
// Notice how the functor, in its body, uses S.add. It takes the implementation of add from S and
// uses it to implement of_list (and the same for empty), thus solving the exact problem we had
// before when we tried to use includes.

// When we apply SetOfList to our set implementations, we get modules containing an of_list function
// for each implementation:

module ListSet: Set = {
  type t<'a> = list<'a>
  let empty = list{}
  let mem = List.mem
  let add = List.cons
  let elements = s => List.sort_uniq(compare, s)
}

module UniqListSet: Set = {
  // all values in the list must be unique
  type t<'a> = list<'a>
  let empty = list{}
  let mem = List.mem
  let add = (. x, s) => {
    mem(x, s) ? s : list{x, ...s}
  }
  let elements = Stdlib.Function.identity
}

module OfList = SetOfList(ListSet)
module UniqOfList = SetOfList(UniqListSet)

// The functor has enabled the code reuse we couldn’t get before: we now can implement a single
// of_list function and from it derive implementations for two different sets.

// But that’s the only function those two modules contain. Really what we want is a full set
// implementation that also contains the of_list function. We can get that by combining includes
// with functors:

module SetWithOfList = (S: Set) => {
  include S
  let of_list = lst => {
    List.fold_right(S.add, lst, S.empty)
  }
}

// That functor takes a set as input, and produces a module that contains everything from that set
// (because of the include) as well as a new function of_list.

// When we apply the functor, we get a very nice set module:
module SetL = SetWithOfList(ListSet)
module UniqSetL = SetWithOfList(UniqListSet)

// Notice how the output structure records the fact that its type t is the same type as the type t
// in its input structure. They share it because of the include.

// Stepping back, what we just did bears more than a passing resemblance to class extension in Java.
// We created a base module and extended its functionality with new code while preserving its old
// functionality. But whereas class extension necessitates that the newly extended class is a
// subtype of the old, and that it still has all the old functionality, OCaml functors are more
// fine-grained in what they can accomplish. We can choose whether they include the old
// functionality. And no subtyping relationships are necessarily involved. Moreover, the functor we
// wrote can be used to extend any set implementation with of_list, whereas class extension applies
// to just a single base class. There are ways of achieving something similar in object-oriented
// languages with mixins, which enable a class to re-use functionality from other classes without
// necessitating the complication of multiple inheritance.
