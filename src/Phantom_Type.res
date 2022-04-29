//
// https://moondaddi-dev.translate.goog/posts/phantom_type/?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=en&_x_tr_pto=wapp
//
// References:
// Phantom Types in ReasonML:
//   https://medium.com/reasontraining/phantom-types-in-reasonml-1a4cfc18d999
//   https://gist.github.com/busypeoples/3a28d039272ec3eb33ca2fc6b32dafc7
//

@@warning("-27-32")

//
//  What is Phantom type?
//

// A phantom type is a type whose type parameter exists only on the left side of the type declaration part. Let's express it in code.

type t<'a> = string
type dog
type cat

let mike: t<dog> = "Mike"
let marla: t<cat> = "Marla"

// The compiler treats mike: t<dog> and marla: t<cat> as different types. Actually, it contains a string corresponding to the string type. A type whose type parameter exists only on the left side of the declaration section and can be treated as another type is called a phantom type.

module type Animal = {
  type t<'a>
  type dog
  type cat
  let makeDog: string => t<dog>
  let makeCat: string => t<cat>

  let mate: (t<'a>, t<'a>) => string

  let interMate: (t<'a>, t<'b>) => string
}

module Animal: Animal = {
  type t<'a> = string
  type dog
  type cat
  let makeDog = a => a
  let makeCat = a => a

  let mate = (a, b) => `${a} and ${b} are now friends`

  let interMate = (a, b) => `${a} and ${b} are now friends`
}

let mike = Animal.makeDog("Mike")
let marla = Animal.makeCat("Marla")

// Js.Console.log(Animal.mate(mike, marla))
// Error /** This has type: Animal.t<Animal.cat> Somewhere wanted: Animal.t<Animal.dog>  The incompatible parts: Animal.cat vs Animal.dog */

Animal.interMate(mike, marla)->Js.log

// interMate Since the function is defined as a function that receives two arguments of type t<'a> and t<'b> and returns a string, it is compiled even if mike and is passed as an argument. marla

// If you use a phantom type, you can have the effect of having string multiple subtypes ( t<'a>, ) for one type ( ).t<'b>

// to summarize,
// * A type that has a type parameter, but is only on the left side of the declaration.
// * For one data representation (here, mike and marla, which are strings), you can have subtypes.

//
// Usage example
//

// The Animal example described earlier is an example that is often cited in reference materials explaining the phantom type. Let's take a slightly more specific example.

// Form data is often used when creating applications. Let's implement the part that verifies this form data using the phantom type.

module type FormData = {
  type t<'a>
  type validated
  type unvalidated

  let make: string => t<unvalidated>
  let validate: t<unvalidated> => t<validated>
  let saveToDB: t<validated> => unit
}

module FormData: FormData = {
  type t<'a> = string
  type validated
  type unvalidated

  let make = a => a
  let validate = a => a
  let saveToDB = a => ()
}

// We will use the FormData module implemented like this.
let shouldBeOkay = FormData.make("should be okay")
let validatedData = FormData.validate(shouldBeOkay)
FormData.saveToDB(validatedData)

// Using the phantom type, you can force it to be passed as an argument to validate the function only after passing through the function. saveToDBIf validate you don't go through the function, this code won't compile.

let cantBePassed = FormData.make("ok?")
// FormData.saveToDB(cantBePassed) // error since it isn't a validated type

// Let's say that the FormData module you created is shared with other fellow developers. In that case, what if a fellow developer accidentally (?) or intentionally validate creates a function that bypasses the ?

// let byPass: string => FormData.t<FormData.validated> = a => a

// Fortunately, it doesn't compile. Because, as declared in the FormData module type, string and FormData.t<'a> are completely different types. Of course, the implementation, FormData module, declared t<'a> as string and implemented the make function validate, but since the bypass function is not implemented in the FormData module, the compiler warns that the bypass function is a type error depending on the module type. is.

// validateFunctions that operate using FormData.t<'a> classified using phantom types are saveToDB made unusable by using values manipulated externally.

// finish
// We looked at two examples of phantom types. It is said that Phantom Type is not only in ReScript/ReasonML, but can also be used in other languages such as Haskell, Rust, and Swift. Actually, it is not necessary to know the phantom type because it belongs to the art of using the type system, but it seems that you can make a little more fun and safe code using the type.
