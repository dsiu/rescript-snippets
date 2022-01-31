//
// Type Alias
//
type scoreType = int
let x: scoreType = 10

//
// Type Parameter (Aka Generic)
//

// this is a tuple of 3 items, explained next
type intCoordinates = (int, int, int)
type floatCoordinates = (float, float, float)

let a1: intCoordinates = (10, 20, 20)
let b1: floatCoordinates = (10.5, 20.5, 20.5)

type coordinates<'a> = ('a, 'a, 'a)

let a2: coordinates<int> = (10, 20, 20)
let b2: coordinates<float> = (10.5, 20.5, 20.5)

// Note that the above codes are just contrived examples for illustration purposes. Since the types are inferred, you could have just written:
let buddy_int = (10, 20, 20)
let buddy_float = (10.1, 20.2, 20.5)

// inferred as `array<string>`
let greetings = ["hello", "world", "how are you"]

// Types can receive many arguments, and be composable.
type result<'a, 'b> =
  | Ok('a)
  | Error('b)

type myPayload = {data: string}

type myPayloadResults<'errorType> = array<result<myPayload, 'errorType>>

let payloadResults: myPayloadResults<string> = [
  Ok({data: "hi"}),
  Ok({data: "bye"}),
  Error("Something wrong happened!"),
]

// Recursive Types
type rec person = {
  name: string,
  friends: array<person>,
}

// Mutually Recursive Types
type rec student = {taughtBy: teacher}
and teacher = {students: array<student>}

// Type Escape Hatch
// ReScript's type system is robust and does not allow dangerous, unsafe stuff like implicit type casting, randomly guessing a value's type, etc. However, out of pragmatism, we expose a single escape hatch for you to "lie" to the type system:

// external myShadyConversion: myType1 => myType2 = "%identity"

// This declaration converts a myType1 of your choice to myType2 of your choice. You can use it like so:
external convertToFloat: int => float = "%identity"
let age = 10
let gpa = 2.1 +. convertToFloat(age)
