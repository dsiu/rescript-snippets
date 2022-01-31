// Records are like JavaScript objects but:
// are immutable by default
// have fixed fields (not extensible)

// Type Declaration
type person = {
  age: int,
  name: string,
}

// Creation
let me = {
  age: 5,
  name: "Big ReScript",
}

// When you create a new record value, ReScript tries to find a record type declaration that conforms to the shape of the value. So the me value here is inferred as of type person.
// The type is found by looking above the me value. Note: if the type instead resides in another file or module, you need to explicitly indicate which file or module it is:

// Access
let name = me.name

// Immutable Update
let meNextYear = {...me, age: me.age + 1}

// Mutable Update
// Record fields can optionally be mutable. This allows you to efficiently update those fields in-place with the = operator.
type person2 = {
  name: string,
  mutable age: int,
}

let baby = {name: "Baby ReScript", age: 5}
baby.age = baby.age + 1
