@@warning("-32") // unused value

//
// Polymorphic Variant
//

// Polymorphic variants (or poly variant) are a cousin of variant. With these differences:
//   They start with a # and the constructor name doesn't need to be capitalized.

//   They don't require an explicit type definition. The type is inferred from usage.

//   Values of different poly variant types can share the constructors they have in common (aka, poly variants are "structurally" typed, as opposed to "nominally" typed).

// Creation
let myColor = #red
let myLabel = #"aria-hidden"
let myNumber = #7

// Type Declaration
// Although optional, you can still pre-declare a poly variant type:
type color1 = [#red | #green | #blue]

// These types can also be inlined, unlike for regular variant:
let render = (myColor: [#red | #green | #blue]) => {
  switch myColor {
  | #blue => Js.log("Hello blue!")
  | #red
  | #green =>
    Js.log("Hello other colors")
  }
}

// Note: because a poly variant value's type definition is inferred and not searched in the scope, the following snippet won't error:

let render1 = myColor => {
  switch myColor {
  | #blue => Js.log("Hello blue!")
  | #green => Js.log("Hello green!")
  // works!
  | #yellow => Js.log("Hello yellow!")
  }
}

// That myColor parameter's type is inferred to be #red, #green or #yellow, and is unrelated to the color type. If you intended myColor to be of type color, annotate it as myColor: color in any of the places.

// Constructor Arguments
type account = [
  | #Anonymous
  | #Instagram(string)
  | #Facebook(string, int)
]

let me: account = #Instagram("Jenny")
let him: account = #Facebook("Josh", 26)

// Combine Types and Pattern Match
// You can use poly variant types within other poly variant types to create a sum of all constructors:
type red = [#Ruby | #Redwood | #Rust]
type blue = [#Sapphire | #Neon | #Navy]

// Contains all constructors of red and blue.
// Also adds #Papayawhip
type color = [red | blue | #Papayawhip]

let myColor: color = #Ruby

// There's also some special pattern matching syntax to match on constructors defined in a specific poly variant type:

switch myColor {
| #...blue => Js.log("This blue-ish")
| #...red => Js.log("This red-ish")
| other => Js.log2("Other color than red and blue: ", other)
}

// This is a shorter version of:
switch myColor {
| #Sapphire | #Neon | #Navy => Js.log("This is blue-ish")
| #Ruby | #Redwood | #Rust => Js.log("This is red-ish")
| other => Js.log2("Other color than red and blue: ", other)
}

// Structural Sharing
// Since poly variants value don't have a source of truth for their type, you can write such code:
type preferredColors = [#white | #blue]

let myColor: preferredColors = #blue

let displayColor = v => {
  switch v {
  | #red => "Hello red"
  | #green => "Hello green"
  | #white => "Hey white!"
  | #blue => "Hey blue!"
  }
}

Js.log(displayColor(myColor))
// With a regular variant, the line displayColor(myColor) would fail, since it'd complain that the type of myColor doesn't match the type of v. No problem with poly variant.

// Bind to Functions
// For example, let's assume we want to bind to Intl.NumberFormat and want to make sure that our users only pass valid locales, we could define an external binding like this:
type t

@scope("Intl") @val
external makeNumberFormat: [#"de-DE" | #"en-GB" | #"en-US"] => t = "NumberFormat"

let intl = makeNumberFormat(#"de-DE")
// The JS output is identical to handwritten JS, but we also get to enjoy type errors if we accidentally write makeNumberFormat(#"de-DR").

//
// Extra Constraints on Types
//

// The previous poly variant type annotations we've looked at are the regular "closed" kind. However, there's a way to express "I want at least these constructors" (lower bound) and "I want at most these constructors" (upper bound):

// Only #Red allowed. Closed.
let basic: [#Red] = #Red

// May contain #Red, or any other value. Open
// here, foreground will actually be inferred as [> #Red | #Green]
let foreground: [> #Red] = #Green

// The value must be, at most, one of #Red or #Blue
// Only #Red and #Blue are valid values
let background: [< #Red | #Blue] = #Red
// Note: We added this info for educational purposes. In most cases you will not want to use any of this stuff, since it makes your APIs pretty unreadable / hard to use.

//
// Closed [
// This is the simplest poly variant definition, and also the most practical one. Like a common variant type, this one defines an exact set of constructors.
type rgb = [#Red | #Green | #Blue]

let color: rgb = #Green
// In the example above, color will only allow one of the three constructors that are defined in the rgb type. This is usually the way how poly variants should be defined.

//
// Lower Bound [>
//A lower bound defines the minimum set of constructors a poly variant type is aware of. It is also considered an "open poly variant type", because it doesn't restrict any additional values.
//Here is an example on how to make a minimum set of basicBlueTones extensible for a new color type:
type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a
type color2 = basicBlueTone<[#Blue | #DeepBlue | #LightBlue | #Purple]>

let color: color2 = #Purple

// This will fail due to missing minimum constructors:
// type notWorking = basicBlueTone<[#Purple]>

// Note: Since we want to define an extensible poly variant, we need to provide a type placeholder <'a>, and also add as 'a after the poly variant declaration, which essentially means: "Given type 'a is constraint to the minimum set of constructors (#Blue | #DeepBlue | #LightBlue) defined in basicBlueTone".

//
// Upper Bound [<
// The upper bound works in the opposite way than a lower bound: the extending type may only use constructors that are stated in the upper bound constraint.
type validRed<'a> = [< #Fire | #Crimson | #Ash] as 'a
type myReds = validRed<[#Ash]>

// This will fail due to unlisted constructor not defined by the lower bound
// type notWorking = validRed<[#Purple]>

// Coercion
// You can convert a poly variant to a string or int at no cost:
type company = [#Apple | #Facebook]
let theCompany: company = #Apple

let message = "Hello " ++ (theCompany :> string)
