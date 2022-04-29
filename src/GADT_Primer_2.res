//
// https://moondaddi-dev.translate.goog/posts/what_is_gadt/?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=en&_x_tr_pto=wapp#혼종heterogeneous-리스트
//
@@warning("-27")

//
// ADTs as GADTs
//
// Here's an ADT that represents a simple boolean. boolIt is isomorphic with the type in Reason .
// type bool' = True | False

// GADT allows you to specifically specify the type of data constructor 2 . This is the key point of GADT. So , let's write a GADT that uses and of the above ADT True as types.False bool'
type rec bool' =
  | True: bool'
  | False: bool'

type rec option'<'a> =
  | None': option'<'a>
  | Some'('a): option'<'a>

let mapOption' = (f, opt) =>
  switch opt {
  | None' => None'
  | Some'(x) => Some'(f(x))
  }

let a = Some'(5)
let b = None'
let inc = x => x + 1
let c = a |> mapOption'(inc)
let d = b |> mapOption'(inc)

//
// Unleashing GADTs
//
// Again, this is very important for understanding GADT. Allows you to specify the type of data constructor specifically . We'll use the function to encode additional information about the type.

// polymorphic return
// ADT Version
type prim_adt =
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Str(string)

// Our goal is primto write a function that, given a value of a type, returns the primitive value it holds.
/* eval(Int(42)) => 42  eval(Float(4.2)) => 4.2  eval(Bool(false)) => false  eval(Str("Hello")) => "Hello" */

// prim The type is expressed in GADT style as follows.
type rec prim<'a> =
  | Int(int): prim<int>
  | Float(float): prim<float>
  | Bool(bool): prim<bool>
  | Str(string): prim<string>

// Let me explain a few things.
// * We made the type polymorphic 'a by specifying .prim
// * You can give the data constructors prim<'a> a more specific type by specifying the type instead. Otherwise, it's no different than ADT.
// * Int(int)As in the case of , prim<'a> instead prim<int> of specifying the type, other data constructors are also specified. In this way, we have encoded additional information about the type together.
// * 'a 'a It is similar to the one used for the phantom type in that an explicit type is given to all positions corresponding to . GADT is also called first class phantom type.

// Now we're going to write a function that prim returns a primitive value for a type. eval

let eval:
  type a. prim<a> => a =
  prim =>
    switch prim {
    | Int(i) => i
    | Float(f) => f
    | Bool(b) => b
    | Str(s) => s
    }

// Notice a the use of as a scoped type variable . type a. In the code above type a.you can read it like this:
//
// for all types a
//
// So the full signature can be read like this:
//
// For all types a , it takes a prim<'a> type as an argument and returns a value of type a.
//
// type a. By adding, a you can bring it into scope and use it as a return value.
//

// We can now use eval functions that return different primitive values depending on the input value, and polymorphic returns.

let myInt = eval(Int(42))
let myFloat = eval(Float(4.2))
let myBool = eval(Bool(false))
let myStr = eval(Str("Hello"))

// Such a function can not eval be implemented with the above ADT .

//
// Heterogeneous List
//
// for rescript: https://forum.rescript-lang.org/t/how-to-define-heterogeneous-list/1015/1
//
// Reason's list type can have only one type of values. We can have a list of strings or a list of integers, but we can't have a list that has both strings and integers.
// Some lists can be defined as empty lists or lists combined with other items. ( consIt can be expressed as .)

// Reason code is used as it is for faithful delivery of the body.

module List' = {
  type rec t<'a> =
    | Empty
    | Con('a, t<'a>)
}

let myList = {
  open List'
  Con(1, Con(2, Con(3, Empty)))
}

myList->Js.log2("myList")

// Here, all items in the list 'a must have the same type . However, we can implement a hybrid list using GADT . Here, every item in the list can be of a different type.

module HList = {
  type rec t =
    | Empty: t
    | Con('a, t): t

  let rec length: t => int = t => {
    switch t {
    | Empty => 0
    | Con(_, t) => 1 + length(t)
    }
  }
}

let myHeteroList = {
  open HList
  Con(1, Con(2.5, Con(false, Con("abc", Con(Some(5), Con(Con(1, Con("def", Empty)), Empty))))))
}

let myListLength = HList.length(myHeteroList)

myHeteroList->Js.log2("myHeteroList")
myListLength->Js.log2("myListLength")

//
// A little closer to the dependency type program
//

// We know that a value depends on its type. A dependent type is a type whose definition depends on a value. Reason has no dependent types, but it can help GADT to further constrain types and their values ​​to each other. We can pass additional information to the GADT about different values ​​at the type level, mimicking the role played by dependent types in specific contexts.

// safe list
// In Reason, the headfunction (Belt.List.head) optionreturns a type. If the list has at least one value, we get Some(value)otherwise None.

// Our goal is to implement a function that doesn't compile for an empty list and otherwise returns the first item that isn't of type option . headTo do that, we still have to pass the same list type, 비어있는or 비어있지 않은some additional information to distinguish the list. First , we will implement empty and non empty types that can distinguish between lists 비어있는or lists.비어있지 않은

module SafeList = {
  type empty = Empty
  type nonEmpty = NonEmpty

  type rec t<'a, 's> =
    | Empty: t<'a, empty>
    | Con('a, t<'a, 's>): t<'a, nonEmpty>

  let rec length:
    type s. t<_, s> => int =
    t => {
      switch t {
      | Empty => 0
      | Con(_, t) => 1 + length(t)
      }
    }

  let head:
    type a. t<a, nonEmpty> => a =
    t => {
      switch t {
      | Con(h, _) => h
      }
    }
}

// Looking at a few
//
// * Compared to the existing list type, it passes an additional type parameter.
// * It takes an extra parameter to tell if the list is empty or non-empty.
// * Our head function is nonEmpty only defined for types, so empty it doesn't work on lists.
// * headUnlike functions, length functions operate on all types of s, empty or nonEmpty on all lists.

let nonEmptyList = {
  open SafeList
  Con(1, Con(2, Con(3, Con(4, Empty))))
}
let sizeOfNonEmptyList = SafeList.length(nonEmptyList)
let firstElem = SafeList.head(nonEmptyList)

sizeOfNonEmptyList->Js.log2(_, "sizeOfNonEmptyList")
firstElem->Js.log2("firstElem")

//

let emptyList = {
  open SafeList
  Empty
}

let sizeOfEmptyList = SafeList.length(emptyList)
emptyList->Js.log2("emptyList")
sizeOfEmptyList->Js.log2("sizeOfEmptyList")
// let firstElemOfEmptyList = SafeList.head(emptyList)
// head The function will not compile for an empty list.

//
// Peano Numbers
//
// We can SafeLists extend the implementation of , so that we can tell not only if it is empty, but also how many items it is made of. To implement such a list, we must first implement Peano numbers .

// Peano numbers are a simple way to express natural numbers using only zero (0) and a successor function. We can define zero and all subsequent natural numbers.

type zero = Zero
type succ<'a> = Succ('a)
type rec nat<_> =
  | Zero: nat<zero>
  | Succ(nat<'a>): nat<succ<'a>>

// We can now represent natural numbers at the top level .
type one_ = nat<succ<zero>>
type two_ = nat<succ<succ<zero>>>
type three_ = nat<succ<succ<succ<zero>>>>

// Going further, we can reflect these ties as values.
let one: one_ = Succ(Zero)
let two: two_ = Succ(Succ(Zero))
let three: three_ = Succ(two)

one->Js.log2("one")
two->Js.log2("two")

// incA function that adds one Succc an also be written by adding
let inc: nat<'a> => nat<succ<'a>> = pn => Succ(pn)
let three_ = inc(Succ(Succ(Zero)))
let three__ = inc(two)

// dec A function is a function that subtracts one only for non-zero types. dec does not compile for type zero. It's like dealing with only natural numbers.
let dec: nat<succ<'a>> => nat<'a> = pn =>
  switch pn {
  | Succ(a) => a
  }
let one_ = dec(Succ(Succ(Zero)))

// let _ = dec(dec(one))
/* This has type: nat<zero>  Somewhere wanted: nat<succ<'a>>   The incompatible parts:  zero vs succ<'a> */

// The uniform function can be implemented as recursive pattern matching.
let rec isEqual:
  type a b. (nat<a>, nat<b>) => bool =
  (i, j) =>
    switch (i, j) {
    | (Zero, Zero) => true
    | (Succ(n), Succ(m)) => isEqual(n, m)
    | (_, _) => false
    }

let isTwoEqualToOne = isEqual(one, two)
let isThreeEqualToSuccTwo = isEqual(Succ(two), three)

isTwoEqualToOne->Js.log2("isTwoEqualToOne")
isThreeEqualToSuccTwo->Js.log2("isThreeEqualToSuccTwo")

// Finally, eval you can write a function that converts to an integer.
let rec eval:
  type a. nat<a> => int =
  pn =>
    switch pn {
    | Zero => 0
    | Succ(m) => 1 + eval(m)
    }

let threeValue = eval(three)
let fourValue = eval(Succ(three))
threeValue->Js.log2("threeValue")
fourValue->Js.log2("fourValue")

//
// list of known length
//
// Now our goal is to implement a list of known length. We know how to represent natural numbers at the type level using Peano numbers, and we can use them to pass length information to types.

module LengthList = {
  type rec t<'a, _> =
    | Empty: t<'a, nat<zero>>
    | Con('a, t<'a, nat<'l>>): t<'a, nat<succ<'l>>>

  let rec length:
    type a. t<_, a> => int =
    l => {
      switch l {
      | Empty => 0
      | Con(_, t) => 1 + length(t)
      }
    }

  let pop:
    type a. t<_, nat<succ<a>>> => t<_, nat<a>> =
    l => {
      switch l {
      | Con(_, xs) => xs
      }
    }

  let push:
    type a. (t<_, nat<a>>, _) => t<_, nat<succ<a>>> =
    (l, v) => {
      switch l {
      | Empty => Con(v, Empty)
      | xs => Con(v, xs)
      }
    }
}

// There are many things to point out here.
//
// As with SafeList, we provide the list type with an additional type parameter containing the size of the list.
// * emptyThe size of the list is 0, and in cons the case of (::), the list size is succ.
// * popIn a function, the nat(succ(a))type signature specifies that the list passed as an argument must have at least one item. And nat(a)by using the type, the return value will be one smaller size.
// * push Similarly for functions, there nat(a)is no restriction on the length of the list passed as an argument using a type, but the returned list has a nat(succ(a))type.
// * push The implementation of the function guarantees type safety that it will be a list of lengths matching the information encoded in the type. popAs with functions, pop functions will not compile against an empty list.

let twoElemList = {
  open LengthList
  Con(1, Con(2, Empty))
}

let threeElemList = {
  open LengthList
  push(twoElemList, 3)
}

let oneElemList = {
  open LengthList
  pop(pop(threeElemList))
}
twoElemList->Js.log2("twoElemList")
threeElemList->Js.log2("threeElemList")
oneElemList->Js.log2("oneElemList")

// let _ = LengthList.pop(pop(oneElemList)) // This will not compile.

// pushBy placing length constraints on the and pop unctions using type parameters, the resulting code is the only possible implementation. With normal list types, the push_ function below can pass the type checker, which is obviously wrong. LengthList By giving length information to the type in , you can ensure a more secure implementation.

let push_: (list<'a>, 'a) => list<'a> = (l, v) =>
  switch l {
  | list{} => list{v}
  | xs => list{v} // this is mistake by programmer but it is ok for compiler
  }

let _ = push_(list{1, 2, 3}, 4)

// Tightening the type constraint has obvious benefits, but sometimes a compromise is necessary. For example, it is impossible to write a function for LengthList.filter
