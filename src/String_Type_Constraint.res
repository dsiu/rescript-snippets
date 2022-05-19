module type S = {
  type t
  let make: string => t
}

module Username: S = {
  type t = string
  let make = s => s
}

module Password: S = {
  type t = string
  let make = s => s
}

let f = (u: Username.t, pw: Password.t) => {
  u->Js.log
  pw->Js.log
}

//let g = () => {
//  let u: Username.t = "dsiu"
//  let p = u
//  f(u, p)
//}

//f("dsiu", "hi")
//f("dsiu"->Username.make, "bbb"->Username.make)->Js.log
let u1 = Username.make("dsiu")
let p1 = Password.make("diu")
//f(p1, u1) // compiler error, which is good since password isn't username
f(u1, p1)
