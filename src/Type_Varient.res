// Most data structures in most languages are about "this and that". A variant allows us to express "this or that".

type myResponse =
  | Yes
  | No
  | PrettyMuch

let areYouCrushingIt = Yes

// myResponse is a variant type with the cases Yes, No and PrettyMuch, which are called "variant constructors" (or "variant tag"). The | bar separates each constructor.

// Constructor Arguments
type account =
  | None
  | Instagram(string)
  | Facebook(string, int)

let myAccount = Facebook("Josh", 26)
let friendAccount = Instagram("Jenny")

// Labeled Variant Payloads (Inline Record)
type user =
  | Number(int)
  | Id({name: string, password: string})

let me = Id({name: "Joe", password: "123"})

// This is technically called an "inline record", and only allowed within a variant constructor. You cannot inline a record type declaration anywhere else in ReScript.

// Of course, you can just put a regular record type in a variant too:
type u = {name: string, password: string}
type user1 =
  | Number(int)
  | Id(u)

let me1 = Id({name: "Joe", password: "123"})

// Variants Must Have Constructors
//If you come from an untyped language, you might be tempted to try type myType = int | string. This isn't possible in ReScript; you'd have to give each branch a constructor: type myType = Int(int) | String(string). The former looks nice, but causes lots of trouble down the line.

// Variant Types Are Found By Field Name
//Please refer to this record section. Variants are the same: a function can't accept an arbitrary constructor shared by two different variants. Again, such feature exists; it's called a polymorphic variant. We'll talk about this in the future =).
