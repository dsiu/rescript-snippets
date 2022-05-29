// Sources
// https://stackoverflow.com/questions/58964775/pattern-matching-on-a-gadt-fails

// GADTs in rescript.

// GADTs are cool. Here's an example of a function with polymorphic return type.
// The thing is that the syntax is buried somewhere I don't remember, and the
// docs won't say anything because this is sort of black magic. Apply with care.

type rec prim<'t> =
  | Int(int): prim<int>
  | String(string): prim<string>
  | Bool(bool): prim<bool>

// The type expression (type t) is known as a locally abstract type. It can be
// read as `forall t. prim<t> -> t`. Without it, pattern matching will fail
// because of type inference issues, the compiler won't know what to do with
// this.
let eval = (type t, p: prim<t>): t => {
  switch p {
  | Int(i) => i
  | String(s) => s
  | Bool(s) => s
  }
}

let t1 = eval(Int(3))
let t2 = eval(String("asdf"))
let t3 = eval(Bool(true))

// Another example with a recursive evaluator.
type rec expr<'t> =
  | Int(int): expr<int>
  | Bool(bool): expr<bool>
  | Eq(expr<int>, expr<int>): expr<bool>
  | Add(expr<int>, expr<int>): expr<int>
  | Mul(expr<int>, expr<int>): expr<int>

// Here with the mighty full explicitly polymorphic locally abstract type annotation.
let rec eval:
  type t. expr<t> => t =
  expr => {
    switch expr {
    | Int(i) => i
    | Bool(b) => b
    | Eq(i1, i2) => eval(i1) == eval(i2)
    | Add(i1, i2) => eval(i1) + eval(i2)
    | Mul(i1, i2) => eval(i1) * eval(i2)
    }
  }

let isOneEqToOne = Eq(Int(1), Int(1))
let shouldBeTrue = eval(isOneEqToOne)

let is2mult2plus3equalTo7 = Eq(Add(Mul(Int(2), Int(2)), Int(3)), Int(7))
let shoulBeAlsoTrue = eval(is2mult2plus3equalTo7)

Js.log(shouldBeTrue)
Js.log(shoulBeAlsoTrue)
