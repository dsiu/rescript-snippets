/**
 How can get keys of object in generic
 https://forum.rescript-lang.org/t/how-can-get-keys-of-object-in-generic/1996/15
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
