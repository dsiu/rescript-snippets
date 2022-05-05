// https://rescript-lang.org/blog/bucklescript-release-7-0-2
//
// [unboxed]
//
@@warning("-32")

// Unboxed variants:
@unboxed
type t = A(int)
let x = A(3)

// Unboxed Records (1 field only)
type t2 = {f: string}
let x = {f: "foo"}

// Another use case is for expressing high rank polymorphism without cost:
@unboxed
type r = {f: 'a. 'a => 'a}
let map_pair = (r, (p1, p2)) => (r.f(p1), r.f(p2))

// Note: 'a. 'a => 'a describes a polymorphic function interface, where 'a can be called with many different types (e.g. f(1) and f("hi")). The compiler will not try to lock 'a for the first type it sees (e.g. the int) on the first call site. The parameter 'a is therefore polymorphic!

// By unboxing those records with one polymorphic function, we will get rid of value restriction for our existing encoding of uncurried function, this will be a major feature!

// Unboxed GADTs:
// As you can already tell, this feature will give us way better possibilities to do interop with polymorphic array representations in JavaScript (without losing any type safetiness!).
// As a more concrete use-case, this will give users the possibility to define types such as int_or_string.

@unboxed
type rec t_gadt = Any('a): t_gadt

let array = [Any(3), Any("a")]

//
// https://rescript-lang.org/blog/union-types-in-bucklescript
//
// To conclude: thanks to unboxed attributes and the module language, we introduce a systematic way to convert values from union types (untagged union types) to algebraic data types (tagged union types). This sort of conversion relies on user level knowledge and has to be reviewed carefully. For some cases where classify is not needed, it can be done in a completely type safe way.

//type rec t3 = Any('a): t3

//@unboxed
//let a = (v: int) => Any(v)
//let b = (v: string) => Any(v)
//let c = (v: bool) => Any(v)

module Number_or_string: {
  type t
  type case =
    | Number(float)
    | String(string)
  let number: float => t
  let string: string => t
  let classify: t => case
} = {
  @unboxed
  type rec t = Any('a): t
  type case =
    | Number(float)
    | String(string)
  let number = (v: float) => Any(v)
  let string = (v: string) => Any(v)
  let classify = (Any(v): t): case =>
    if Js.typeof(v) == "number" {
      Number((Obj.magic(v): float))
    } else {
      String((Obj.magic(v): string))
    }
}

let a = Number_or_string.number(3.14)
let b = Number_or_string.string("hello")
let c = [a, b]
Belt.Array.map(c, x => x->Js.log->ignore)->ignore

module A_or_b: {
  type t
  type a
  type b
  let a: a => t
  let b: b => t
  type case =
    | A(a)
    | B(b)
  let classify: t => case
} = {
  @unboxed
  type rec t = Any('a): t
  type a = array<int>
  type b = list<string>
  type case =
    | A(a)
    | B(b)
  let a = (v: a) => Any(v)
  let b = (v: b) => Any(v)
  let instanceof = %raw(`
    function(a) {
      return  a instanceof globalThis.A;
      }
    `)

  let classify = (Any(v): t) =>
    if instanceof(v) {
      A((Obj.magic(v): array<int>))
    } else {
      B(Obj.magic(b))
    }
}
