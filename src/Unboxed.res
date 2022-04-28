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
