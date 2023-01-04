/*
  Let's Talk About Functors In ReScript
  https://dusty.phillips.codes/2021/09/18/lets-talk-about-functors-in-rescript/
*/

// generic types
type keyValuePair<'a, 'b> = {
  name: 'a,
  value: 'b,
}

let hundredsOfApples: keyValuePair<string, int> = {
  name: "apple",
  value: 480,
}

// You can also create a specific type from a generic type:
type twoStrings = keyValuePair<string, string>

// And you can even create a new generic type from an existing generic type. This one only has one type parameter:
type stringValuePair<'a> = keyValuePair<string, 'a>

// You can pass a generic type into a function and have it return a generic type as the response. For example, this is a valid ReScript function:
let extractValue: keyValuePair<'a, 'b> => 'b = pair => pair.value

let valueIfName: (keyValuePair<string, 'b>, string) => option<'b> = (pair, nameToMatch) => {
  if pair.name == nameToMatch {
    Some(pair.value)
  } else {
    None
  }
}

// You can make a generic function in ReScript where the argument itself is generic like this:
// This function will work by passing in any type. However, there is no way to say you want to specialize that function
let showAndReturn: 'a => 'a = argument => {
  Js.log(argument)
  argument
}

// When we first started this discussion, we noted that a functor is “a function that accepts a module and returns a module.” But I don’t find that a very useful description. Instead, I think of a functor as a “generically typed module.” This is a pretty limited definition compared to the entirety of things a functor can do, but it is probably the definition that is used most often.

type friend = {
  name: string,
  age: int,
}

type dog = {
  name: string,
  colour: string,
}

type book = {
  title: string,
  author: string,
}

// One less-than-desirable way we could implement this is by having a module that exposes addDog, addFriend, and addBook but not the general add function, as follows:
module type DatabaseInterface = {
  let addDog: dog => unit
  let addFriend: friend => unit
  let addBook: book => unit
}

module Database: DatabaseInterface = {
  @module("database") external add: (string, 'a) => unit = "add"

  let addDog = dog => add("dogs", dog)
  let addFriend = friend => add("friends", friend)
  let addBook = book => add("books", book)
}

Database.addDog({name: "lassie", colour: "red and white"})

// Functor
type schemaId = string

module type SchemaItem = {
  type t
  let tableName: schemaId
}

module FriendSchema = {
  type t = friend
  let tableName = "friends"
}

module DogSchema = {
  type t = dog
  let tableName = "dogs"
}

module BookSchema = {
  type t = book
  let tableName = "books"
}

// The syntax for a functor is kind of like a mashup of the syntax for defining a module and the syntax for defining a function. The functor “signature” looks exactly like a function call with parameters and a fat arrow, except that the name must be capitalized and you use a module keyword instead of a let to do the assigning:

// In this case, the “argument” is another module, which must be an instance of the SchemaItem type, but is otherwise completely generic. You cannot pass arguments into a functor that are not modules, but you can pass multiple modules into one.
//The contents of the functor body (the {}) are more like a module definition than a function. Instead of the last executed expression being the “return value” of the functor, the entire body is the returned module. It contains type and let definitions for an entire new module that is (effectively) “generated” on the fly. Here’s one that specifies a couple of simple functions:

module type MakeSchemaType = (Schema: SchemaItem) =>
{
  let add: Schema.t => unit
  let get: int => option<Schema.t>
}

module MakeSchema: MakeSchemaType = (Schema: SchemaItem) => {
  module Database = {
    @module("database") external add: (string, 'a) => unit = "add"
    @module("database") external get: int => 'a = "get"
  }
  let add: Schema.t => unit = item => Database.add(Schema.tableName, item)
  let get: int => option<Schema.t> = id => Database.get(id)
}

//This presupposes the existence of a `Database` module that has bindings
//something like this:

module Friends = MakeSchema(FriendSchema)
module Dogs = MakeSchema(DogSchema)
module Books = MakeSchema(BookSchema)

Friends.add({name: "lizzi", age: 8})

// compile error
// Friends.add({title: "Pyramids", author: "Terry Pratchett"})

/* ================================================================ */
/* ================================================================ */
/* ================================================================ */
/* ================================================================ */

/**
 How can get keys of object in generic
 https://forum.rescript-lang.org/t/how-can-get-keys-of-object-in-generic/1996/15

 keywords: polymorphic, module functor
*/

//
// Ok. How can I write correct interop with firebase? With correct types.
//

//    type User = {
//      name: string
//      age: number
//    }
//    ...
//    const users = firestore.collection('users').orderBy('name')

//
// Functor Solution
//
//
// hoichi
//
//Well, here’s how I might do it with some opaque types and functors 6 (which, as you might guess by the state of the docs, are not easily recommended for newcomers, being a rather advanced technique, but they can be really useful):

type firestore

module type CollectionItem = {
  type t
  type field
}

module Collection = (Item: CollectionItem) => {
  type t
  type item = Item.t

  @send external orderBy: (t, Item.field) => t = "orderBy"
  @send external asList: t => array<Item.t> = "asList"
}

module User = {
  type t = {name: string, age: int}
  type field = [#name | #age]
}

module UsersCollection = Collection(User)

module Collections = {
  @send
  external getUsers: (firestore, @as("users") _) => UsersCollection.t = "collections"
}

let doSome = (firestore: firestore) => {
  open UsersCollection
  firestore->Collections.getUsers->orderBy(#name)->asList
}

//
// Solution 2
//
// tsnobip
// for the sake of exhaustiveness, there are other solutions that don’t involve functors like this one:
//
module Solution2 = {
  type firestore

  type collection<'item, 'orderBy>

  module Collection = {
    @send
    external orderBy: (collection<'item, 'orderBy>, 'orderBy) => collection<'item, 'orderBy> =
      "orderBy"
    @send external asList: collection<'item, _> => array<'item> = "asList"
  }

  module User = {
    type t = {name: string, age: int}
    type collection = collection<t, [#name | #age]>
    @send
    external getUsers: (firestore, @as("users") _) => collection = "collections"
    include Collection
  }

  let doSome = (firestore: firestore): array<User.t> => {
    open User
    firestore->getUsers->orderBy(#name)->asList
  }
}
