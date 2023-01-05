//
// https://mmhaskell.com/monads/functors
//
let log = Js.log

// WHAT IS A FUNCTOR?
//
// A Functor is any type that can act as a generic container. A Functor allows us to transform the
// underlying values with a function, so that the values are all updated, but the structure of the
// container is the same. This is called "mapping".
//
// HOW ARE FUNCTORS REPRESENTED IN HASKELL?
//
// Haskell represents the concept of a functor with the Functor typeclass. This typeclass has a
// single required function fmap.

// class Functor f where
//  fmap :: (a -> b) -> f a -> f b
