//
// ref: https://forum.rescript-lang.org/t/type-as-a-value-undocumented-syntax/3123/2
// ref: https://github.com/bloodyowl/rescript-request/blob/main/src/Request.res

@@warning("-32")

type rec responseTypeGADT<'response> = JsonAsAny: responseTypeGADT<'response>

// This is called GADT (Generalized Algebraic Data Type). This isn’t mentioned anywhere in the documentation most probably because it is an advanced topic.
//
// If you’re familiar with Haskell/Ocaml then it works very similar to GADT in those languages.
//
// GADT allows you to specify the polymorphic variable for each of your constructor. First let’s look at the normal variants or sum types.

// normal variants
// type responseTypeNormal =
//  | JsonBool(bool)
//  | JsonString(string)
//  | JsonAny

// we can then observe the type signature for each of the constructor.
// JsonBool : bool => responseType
// JsonString : string => responseType
// JsonAny : responseType

//They all return the same responseType. What if you want to differentiate the return type?
//Let’s do that.

type responseType2<'response> =
  | JsonBool(bool)
  | JsonString(string)
  | JsonAny

// We added a polymorphic variable 'response to capture the return type but it’s not being used anywhere in our constructors. (btw, this is called phantom type).
// Then we can do,
let x: responseType2<string> = JsonString("hello")
let y: responseType2<bool> = JsonBool(true)

// Both x and y now have a different type, but you’d have to explicitly annotate the type. Also this isn’t entirely safe.

let x: responseType2<bool> = JsonString("hello") // Oh no!

// What we can do is use GADT to differentiate the return type.
type rec responseType<'response> =
  | JsonBool(bool): responseType<bool>
  | JsonString(string): responseType<string>
  | JsonAny: responseType<'response>

let x = JsonString("hello")
let y = JsonBool(true)

// x has type responseType<string> and y has type responseType<bool>. If you do something like,

// let x: responseType<bool> = JsonString("hello") // we get compile error now. Nice!

//
// Polymorphic add
//
// https://github.com/rescript-lang/playground/blob/master/examples/gadt.ml
// https://github.com/rescript-lang/rescript-compiler/issues/686#issuecomment-243556683
//

%%raw(`
  /*
   * this js function will work under both [string] and [float]
   */
  function add (x,y){
    return x + y;
  }
  `)

type rec kind<_> =
  | String: kind<string>
  | Float: kind<float>

@val external add: (@ignore kind<'a>, 'a, 'a) => 'a = ""

let () = {
  Js.log(add(Float, 3.0, 2.0))
  Js.log(add(String, "hello, ", "BuckleScript"))
}
