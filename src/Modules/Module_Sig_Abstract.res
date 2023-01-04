// ref:
// https://kevanstannard.github.io/rescript-blog/module-signatures-with-type.html

// Suppose we declare a module signature:
module type SHOW = {
  type t
  let show: t => string
}

// Here we have an abstract type t and it's up to the implmentation module to provide the type.
// Let's declare a person type:
type person = {name: string}

// And a ShowPerson: SHOW module:
//module ShowPerson: SHOW = {
//  type t = person
//  let show = (person: t) => person.name
//}

// Now we can make use of our ShowPerson module:
// let joe: person = {name: "Joe"}

// ShowPerson.show(joe) // see below

// Unfortunately on that last line the reference to joe is causing an error:
// This has type: person
// Somewhere wanted: ShowPerson.t

// To fix this we need to add a with type annotation to our ShowPerson module:
module ShowPerson: SHOW with type t = person = {
  type t = person
  let show = person => person.name
}

// now it works
let joe: person = {name: "Joe"}

ShowPerson.show(joe)->Js.log
