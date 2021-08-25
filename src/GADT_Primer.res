//
// https://sketch.sh/s/yH0MJiujNSiofDWOU85loX/
//

// ADTs as GADTs

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

// Unleashing GADTs
type rec prim<'a> = [
  | #Int(int)
  | #Float(float)
  | #Bool(bool)
  | #Str(string)
]

let eval = a =>
  switch a {
  | #Int(i) => i
  | #Float(f) => f
  | #Bool(b) => b
  | #Str(s) => s
  }

let myInt = eval(#Int(42))
let myFloat = eval(#Float(4.2))
let myBool = eval(#Bool(false))
let myStr = eval(#Str("Hello"))

// Heterogeneous List
//
// for rescript: https://forum.rescript-lang.org/t/how-to-define-heterogeneous-list/1015/1
//
module List' = {
  type rec t<'a> =
    | Empty
    | Con('a, t<'a>)
}

let myList = {
  open List'
  Con(1, Con(2, Con(3, Empty)))
}

// here is the trick defining heterogeneous list in ReScript
// NOT RECOMMANDED in practice
module HList = {
  type rec t<'b, 'c> =
    | Nil: t<'b, 'b>
    | Cons('a, t<'b, 'c>): t<'b, 'a => 'c>
}

let myHList = {
  open HList
  Cons(1, Cons("a", Cons(1.5, Nil)))
}
myHList->Js.log

// alternative
let hList = (1, ("Hello", (1.234, ())))
