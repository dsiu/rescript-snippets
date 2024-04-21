//
// More type classes in OCaml
// https://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html
//
//
// As demonstrated in previous articles on this blog, OCaml comes with a rich module system. Among
// other things it enables developers to write code that is polymorphic over module signatures. As
// such, parameterized modules (aka functors) play a similar role to what type classes do in
// Haskell, and as explained here, it is straightforward to map simple type classes to modules in
// OCaml. In Haskell, type classes are often used as design patterns for structuring programs and
// over the years a taxonomy of type classes inspired by Category Theory has evolved. Standard
// patterns such as functor, monad and monoid have had a strong influence on the design of common
// APIs. In OCaml there is less focus on such idioms. In this post we explore how some of the
// Haskell patterns implemented in terms of type classes can be ported to OCaml and how that impacts
// program design. In particular we cover four commonly used type classes that ships with standard
// Haskell distributions:
//
// * Functor
// * Monoid
// * Applicative Functor
// we* Traversable
//
// For a more comprehensive guide to these patterns and others Typclassopedia serves as a good
// resource. Before tackling the technical aspects it may be worth elaborating a bit on the
// motivation behind introducing these types of abstractions in the first place. Justifications
// fall under different categories:
//
// * API design
// * Code reusability
// * Testability
//
// The first one is about program design - by mapping a data type to a pattern such as applicative
// functor, we obtain a set of combinators for operating on values of that type. Ideally that
// means less time spent on inventing custom operators and figuring out their semantics. When
// multiple libraries share patterns it also increases the likelihood that consumers of anyone of
// those libraries already are familiar with the corresponding combinators. For example looking at
// the map function over some custom data type, one should expect it to have similar properties to
// the function List.map operating on lists. The second point is about code reuse. By writing
// functions that are expressed solely in terms of other module signatures they become reusable in
// different contexts; For instance by implementing the primitive operators for a monoid we get
// additional combinators (such as concat) defined generically for any monoid for free! Thirdly,
// these patterns all come with a set of theoretically well founded properties. As demonstrated by
// some of the examples below, it is also possible to write generic tests that can be used to
// validate concrete implementations of the patterns for different underlying data types.

@@uncurried
@@uncurried.swap

let log = Js.log
let log2 = Js.log2

// compose

// ( << )
let compose: (. (. 'b) => 'c, (. 'a) => 'b, 'a) => 'c = (. f, g, x) => f(g(x))
let \"<<" = compose
// ( >> )
let compose_right = (. f, g, x) => compose(g, f, x)
let \">>" = compose_right

let id = (. x) => x
let const = (. x, _) => x

// Representing the patterns
//
// We use a standard approach for mapping type classes in Haskell to module signatures in OCaml.

//
// Functors
//
// The Functor type class captures the pattern of mapping over the value(s) of some parameterized
// data type.
//
module type FUNCTOR = {
  type t<'a>
  let map: (. (. 'a) => 'b, t<'a>) => t<'b>
}
// In order for a type to qualify as a functor, one need to provide an implementation for map
// (fmap in Haskell) that satisfies the signature.
// For instance, the Functor instance for the list type in Haskell is given by:
//
// instance Functor [] where
//  fmap = map
//
// Here, map is the standard map function over lists. In OCaml we create a module implementing the
// FUNCTOR signature, which for lists may look like:
module ListFunctor: FUNCTOR with type t<'a> = list<'a> = {
  type t<'a> = list<'a>
  let map = (. f, x) => List.map(f, x)
}

/*
 One difference is that the module is named which allows for multiple instances of the same
 signature for the same type to coexist. The with type construct is required in order to be able
 to export the type 'a t specified by the signature. It makes the fact that ListFunctor.t is
 indeed the type list transparent, allowing us to apply ListFunctor.map to ordinary lists.
 */

/*
  An immediate advantage of capturing the functor pattern explicitly via a signature (FUNCTOR) is
  that it enables us to to define an additional parameterized module with tests for validating any
  concrete implementation of the signature:
 */
module TestFunctor = (F: FUNCTOR) => {
  let test_id = x => F.map(id, x) == x

  let test_compose = xs => {
    let f = x => mod(x, 2)
    let g = x => x - 1
    F.map(compose(f, g), xs) == F.map(f, F.map(g, xs))
  }
}

/*
  The tests here correspond to the two functor laws stated above.
  For instance to test ListFunctor we first apply TestFunctor to this module in order to
  retrieve a specialized version:
 */

module TFL = TestFunctor(ListFunctor)

"Test List Functor"->log
TFL.test_id(list{})->(log2("id: P{", _))
TFL.test_id(list{1, 2})->(log2("id: {1,2}", _))
TFL.test_compose(list{})->(log2("compose: {}", _))
TFL.test_compose(list{1, 2, 3})->(log2("compose: {1,2,3}", _))

/*
  The option type in OCaml also forms a functor:
*/
module OptionFunctor: FUNCTOR with type t<'a> = option<'a> = {
  type t<'a> = option<'a>

  let map: (. (. 'a) => 'b, t<'a>) => t<'b> = (. f, x) =>
    switch x {
    | Some(x) => Some(f(x))
    | None => None
    }
}

// And similar to the list example, we get a test module for free:
module TOF = TestFunctor(OptionFunctor)

"Test Option Functor"->log
TOF.test_id(Some(42))->(log2("id: Some(42)", _))
TOF.test_id(None)->(log2("id: None", _))
TOF.test_compose(Some(42))->(log2("compose: Some(42)", _))
TOF.test_compose(None)->(log2("compose: None", _))

//
// Monoids
//

// Monoid is another example of a common pattern where instances can be found for a variety of
// types. In Haskell it's defined as:
//
// class Monoid m where
//   mempty :: m
//   mappend :: m -> m -> m
//

// Any type qualifying as a monoid must have identity value (mempty) and a binary operator (mappend)
// for composing any two elements. The OCaml version can be specified by the following module type:

module type MONOID = {
  type t
  let empty: t
  let append: (. t, t) => t
}

// There are also a few laws that instances should obey:
//
// mappend mempty x        = x
// mappend x mempty        = x
// mappend x (mappend y z) = mappend (mappend x y) z
//
module TestMonoid = (M: MONOID) => {
  let test_left_id = x => M.append(M.empty, x) == x
  let test_right_id = x => M.append(x, M.empty) == x

  let test_assoc = (x, y, z) => {
    M.append(x, M.append(y, z)) == M.append(M.append(x, y), z)
  }
}

// One of the more famous monoids is given by the natural numbers with addition and identity element 0:
module IntAddMonoid: MONOID with type t = int = {
  type t = int
  let empty = 0
  let append = (. a, b) => a + b
}

// Another advantage of formalizing patterns by explicit signatures is that it enables us to define
// derived combinators generically. For example, the append operation from IntAddMonoid can be
// lifted to a sum function accepting a list of integers, adding them together or defaulting to 0
// if the empty list is given:

{
  open IntAddMonoid
  let sum = (. xs) => List.fold_left(append, empty, xs)
  sum(list{1, 10, 102})->(log2("IntAddMonoid.sum {1,10,102}", _))
}

// The scheme can be generalized to operate on any list of monoids. To avoid having to specify the
// implementation manually for each monoid instance, one may construct a module-functor for
// generating extension functions:

module MonoidUtils = (M: MONOID) => {
  include M
  let \"<+>" = (. x, y) => append(x, y) // infix version of append
  let concat = (. xs) => List.fold_left(\"<+>", empty, xs)
}

// Here MonoidUtils takes a MONOID module and re-exports its definition along with two additional
// utility functions, an infix version of append ( <+> ) and concat.

// other example of a monoid is a list, parameterized over any type. In Haskell the instance is given by:
//
// instance Monoid [a] where
//   mempty = []
//   mappend x y = x ++ y
//

// Where (++) is the concatenation operator for lists. In OCaml one could imagine attempting something like:
// Pseudo-code - not valid
/*
module ListMonoid: MONOID with type t = list<'a> = {
  type t = list<'a>
  let empty = list{}
  let append = (x, y) => List.append(x, y)
}
*/

// However it is not possible to directly parameterize modules by types. A work around can be
// achieved by first introducing a dummy module for wrapping the type and passing it along as a
// module parameter:

module type TYPE = {
  type t
}

module ListMonoid = (T: TYPE): (MONOID with type t = list<T.t>) => {
  type t = list<T.t>
  let empty = list{}
  let append = (. xs, ys) => \"@"(xs, ys)
}

// This comes with an obvious disadvantage of having to create specialized versions for each
// concrete list type. Some of the inconvenience is compensated for by explicit type parameters and
// support for local modules, created at run-time. Here's an example implementing concat for lists
// in terms of the generic list monoid:

{
  // existential type parameter
  let _concat = (type a, xs) => {
    module MU = MonoidUtils(
      ListMonoid({
        type t = a
      }),
    )
    MU.concat(xs)
  }

  /* Its signature is inferred as: */
  // val concat : 'a list list -> 'a list
}

// Applicative Functors
//
// An applicative functor has more structure than a regular functor. In Haskell it can be defined as:
//
// class (Functor f) => Applicative f where
//    pure  :: a -> f a
//    (<*>) :: f (a -> b) -> f a -> f b

// The function pure turns a (pure) value into an applicative value and ( <*> ) takes a function
// wrapped inside an applicative along with an applicative value and returns an applicative result
// corresponding to applying the value to the function. The additional constraint ((Functor f) =>
// Applicative f enforces that any type that instantiates the Applicative type class must also be an
// instance of Functor.
//
// In OCaml we can achieve something similar by including the FUNCTOR signature within a new
// signature APPLICATIVE as in:

module type APPLICATIVE = {
  include FUNCTOR
  let pure: (. 'a) => t<'a>
  let apply: (. t<(. 'a) => 'b>, t<'a>) => t<'b>
}
// Here the infix operator ( <*> ) is named apply.

// For a concrete example consider the applicative instance for the list type. Using the ListFunctor
// module from above:
module ListApplicative: APPLICATIVE with type t<'a> = list<'a> = {
  include ListFunctor

  let pure = (. x) => list{x}

  let apply = (. fs, xs) => List.concat(List.map(f => List.map(f, xs), fs))
}

// ListApplicative simply re-exports the implementation of ListFunctor to satisfy the functor part
// of the signature, also mirroring the constraint from the Haskell version.
//
// pure wraps a value in a list. apply takes a list of functions, a list of values and applies each
// function to all elements of the list. Once again we may construct a utility module with some
// extra operators implemented using the primitive functions:

module ApplicativeUtils = (A: APPLICATIVE) => {
  include A
  /**
    map f xs
    (('a -> 'b), 'a t) => 'bt
  */
  let \"<$>" = (. f) => map(f)

  /**
    apply f xs
    (('a -> 'b) t, 'a t) => 'b t
  */
  let \"<*>" = (. f) => apply(f)

  // const <$> x <*> y
  /**
    <* x y
    ('a t, 'b t) => 'a t
  */
  let \"<*" = (. x, y) => const->\"<$>"(x)->\"<*>"(y)

  //  (fun _ y -> y) <$> x <*> y
  /**
    *> x y
    ('a t, 'b t) => 'b t
  */
  let \"*>" = (. x, y) => ((_, y) => y)->\"<$>"(x)->\"<*>"(y)

  // f <$> x <*> y
  /**
    liftA2 f x y
    (('a -> 'b -> 'c), 'a t, 'b t) => 'c t
  */
  let liftA2 = (. f, x, y) => f->\"<$>"(x)->\"<*>"(y)

  // The infix operators are variations of apply and map, liftA2 is for conveniently lifting a
  // regular function of two arguments into a function operating on two applicative values.
}

// By applying ListApplicative to the ApplicativeUtils functor we obtain a concrete module for
// operating on lists:
module LAU = ApplicativeUtils(ListApplicative)

// Producing the following output:
// module LAU :
//    sig
//      type 'a t = 'a ListApplicative.t
//      val map : ('a -> 'b) -> 'a t -> 'b t
//      val pure : 'a -> 'a t
//      val apply : ('a -> 'b) t -> 'a t -> 'b t
//      val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
//      val ( <* ) : 'a t -> 'b t -> 'a t
//      val ( *> ) : 'a t -> 'b t -> 'b t
//      val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
//    end

// Finally let's a take a look at a concrete example to see what the applicative interface actually
// brings in terms of functionality. Say we want to generate some mock data to be used for testing.
// Given the following types:

type offer = [#Ask | #Bid]

type quote = {
  time: int,
  offer: offer,
  ticker: string,
  value: float,
}

// The snippet below produces a list of all possible combinations of some example data by combining
// a set of properties:
{
  let quotes = {
    open LAU

    let makeQuote = (time, offer, ticker, value) => {time, offer, ticker, value}

    makeQuote
    ->\"<$>"(list{1, 2, 3, 4, 5})
    ->\"<*>"(list{#Ask, #Bid})
    ->\"<*>"(list{"XYZ", "ZYK", "ABC", "CDE", "QRZ"})
    ->\"<*>"(list{100., 90., 80., 70.})
  }
  quotes->Belt.List.toArray->(log2("quotes", _))
}

// By composing applications of pure and ( <*> ) we lift functions of arbitrary arity into
// applicative versions. For the list applicative, that means a generalized version of Cartesian
// products.
//
// Another useful instance of an applicative functor is the option type:

module OptionApplicative: APPLICATIVE with type t<'a> = option<'a> = {
  include OptionFunctor

  let pure = (. x) => Some(x)

  let apply: (. t<(. 'a) => 'b>, t<'a>) => t<'b> = (. fo, xo) => {
    switch (fo, xo) {
    | (Some(f), Some(x)) => Some(f(x))
    | _ => None
    }
  }
}
// Here we rely on the OptionFunctor module to manage the functor part. pure returns a value wrapped
// by the Some constructor and apply only produces a value if neither of its arguments are None
// values. As with many other examples of instances, there is basically only one feasible
// implementation to choose from given the type constraints of the function signature.

// With the implementation of the core interface, utilities come for free:

module OAU = ApplicativeUtils(OptionApplicative)

// We can now use it to conveniently lift operations into versions accepting optional arguments.
// Consider the following (safe) versions of division and square-root functions:

let \"//." = (n, d) => {
  d == 0. ? None : Some(n /. d)
}
let ssqrt = x => {
  x < 0. ? None : Some(sqrt(x))
}

{
  // Say we want to implement the formula f(x,y,z) = (x / y) + sqrt(x) - sqrt(y). The obvious
  // approach is to use pattern matching as in:
  let f = (x, y) => {
    switch (\"//."(x, y), ssqrt(x), ssqrt(y)) {
    | (Some(z), Some(r1), Some(r2)) => Some(z +. r1 -. r2)
    | _ => None
    }
  }

  // Using the applicative operators from the OAU module enables an alternative (more succinct) definition:
  let f' = (x, y) => {
    open OAU
    ((z, r1, r2) => z +. r1 -. r2)->\"<$>"(\"//."(x, y))->\"<*>"(ssqrt(x))->\"<*>"(ssqrt(y))
  }

  f(1., 2.)->(log2("f(1.,2.) = ", _))
  f'(1., 2.)->(log2("f'(1.,2.) = ", _))
  f(1., 0.)->(log2("f(1.,0.) = ", _))
  f'(1., 0.)->(log2("f'(1.,0.) = ", _))
  f(1., -2.)->(log2("f(1.,2.) = ", _))
  f'(1., -2.)->(log2("f'(1.,2.) = ", _))
}

// Applicative functors also come with a set of laws. In Haskell expressed as:
//
// -- Identity
// pure id <*> v                 = v
//
// -- Homomorphism
// pure f <*> pure x             = pure (f x)
//
// -- Interchange
// u <*> pure y                  = pure ($ y) <*> u
//
// --- Composition
// pure (.) <*> u <*> v <*> w   = u <*> (v <*> w)

// These may again be turned into a generic testing module:

module TestApplicative = (A: APPLICATIVE) => {
  module AU = ApplicativeUtils(A)

  open AU

  let test_id = x => pure(id)->\"<*>"(x) == x

  let test_hom = (f, x) => pure(f)->\"<*>"(pure(x)) == pure(f(x))

  //  let test_interchange = (u, y) => u->\"<*>"(pure(y)) == pure(f => f(y))->\"<*>"(u)

  let test_composition = (u, v, w) => {
    pure(\"<<")->\"<*>"(u)->\"<*>"(v)->\"<*>"(w) == u->\"<*>"(v->\"<*>"(w))
  }
}

// and be used to validate arbitrary instances of this pattern.
//
// For example to test the list instance, we first construct a concrete module using the TestApplicative functor:
//
module TAL = TestApplicative(ListApplicative)

// This may be used as in:
TAL.test_id(list{})->(log2("test_id = ", _))
//TAL.test_interchange(list{String.length}, "interchange")->log2("test_interchange = ", _)
TAL.test_hom(String.length, "Homomorphism")->(log2("test_hom = ", _))

//
// Traversables
//

// Traversable is an interesting type class which also brings a couple of additional challenges to
// our effort of mapping Haskell patterns to OCaml. It may be described as a generalization of the
// iterator pattern and is defined in Haskell as:
//
//  class (Functor t, Foldable t) => Traversable t where
//    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
//    sequenceA :: Applicative f => t (f a) -> f (t a)
//    mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
//    sequence  ::       Monad m => t (m a) -> m (t a)

// Concrete instances can be written by implementing any one of the above functions as they can all
// be expressed in terms of each other. We could potentially replicate this flexibility in OCaml by
// a set of different module-functors with signatures wrapping each function. However, for the
// purpose of this exercise we settle on traverse as the defining implementation. Traverse is also
// parameterized over an Applicative functor. A first attempt in OCaml might be something along the
// lines of:

// Concrete instances can be written by implementing any one of the above functions as they can all
// be expressed in terms of each other. We could potentially replicate this flexibility in OCaml by
// a set of different module-functors with signatures wrapping each function. However, for the
// purpose of this exercise we settle on traverse as the defining implementation. Traverse is also
// parameterized over an Applicative functor. A first attempt in OCaml might be something along the
// lines of:

// (*Psuedo-code - not valid OCaml!*)
//  module type TRAVERSABLE = sig
//    type 'a t
//    val traverse :  (type a)
//                    (module A : APPLICATIVE with type 'a t = 'a a)
//                    ('a -> 'b a) ->
//                    'a A.t ->
//                    ('b t) a
//  end;;

// However here the type a would itself require a type parameter. In Haskell lingo it is said to
// have kind (* -> *). Unfortunately OCaml does not support higher-kinded polymorphism.
//
//Instead of passing APPLICATIVE as an argument to each invocation of traverse we can embed it in
//the module signature:

module type TRAVERSABLE = {
  type t<'a>
  module Applicative: APPLICATIVE
  let traverse: (. (. 'a) => Applicative.t<'b>, t<'a>) => Applicative.t<t<'b>>
}

// To mimic the Haskell constraints it is tempting to also require the FUNCTOR interface by throwing
// in a extra include FUNCTOR. However, there's a technical reason for why this may not be a good
// idea which we'll return to in a bit.

// Even though the signature references a specific implementation of an APPLICATIVE we can recover
// genericity by relying on module-functors to specify the implementation of a traversable for any
// applicative argument. Let's consider the functor for list traversables:

module ListTraversable = (A: APPLICATIVE): (
  TRAVERSABLE with type t<'a> = list<'a> and type Applicative.t<'a> = A.t<'a>
) => {
  type t<'a> = list<'a>

  module Applicative = A

  let rec traverse: (. (. 'a) => Applicative.t<'b>, t<'a>) => Applicative.t<t<'b>> = (. f, xs) => {
    module AU = ApplicativeUtils(A)
    open AU
    switch xs {
    | list{} => A.pure(list{})
    //    | list{x, ...xs} => \"<*>"(\"<$>"((y, ys) => list{y, ...ys}, f(x)), traverse(f, xs))
    | list{x, ...xs} => f(x)->\"<$>"((y, ys) => list{y, ...ys}, _)->\"<*>"(traverse(f, xs))
    }
  }
}

// Here one has to accept some extra verbosity compared to the Haskell version, although the code
// itself is fairly straightforward. The functor argument A of type APPLICATIVE serves to fulfil the
// requirement of having to export the APPLICATIVE module. The implementation of traverse is the
// interesting bit. Note that it is indeed defined generically for any applicative functor. The
// ApplicativeUtils module constructor comes in handy for accessing the infix versions of the
// operators.

// To give ListTraversable a try, consider how it can be used for option effects:
module LTO = ListTraversable(OptionApplicative)

// This results in a module with the following signature:
//
//  module LTO :
//    sig
//      type 'a t = 'a list
//      val map : ('a -> 'b) -> 'a t -> 'b t
//      module Applicative : sig  end
//      val traverse : ('a -> 'b Applicative.t) -> 'a t -> 'b t Applicative.t
//    end;;

//
// where we also know that the Applicative sub-module is in fact our OptionApplicative.
// traverse in this context is a function that allows us to map each element of a list to an optional
// value where the computation produces a list with all values collected, only in case every element
// was successfully mapped to a Some value.
//
// For example using the safe square root function from above we can transform it into a version
// operating on lists:

{
  let all_roots = LTO.traverse(ssqrt)

  all_roots(list{4.0, 9.0, 16.0})->(log2("all_roots = ", _))

  all_roots(list{4.0, -9.0, 16.0})->(log2("all_roots = ", _))
}

// Next, let's consider a custom type ('a tree) for which we are also able to implement the
// traversable interface:

type rec tree<'a> =
  | Leaf
  | Node(tree<'a>, 'a, tree<'a>)

let node = (l, x, r) => Node(l, x, r)

module TreeTraversable = (A: APPLICATIVE): (
  TRAVERSABLE with type t<'a> = tree<'a> and type Applicative.t<'a> = A.t<'a>
) => {
  module Applicative = A

  type t<'a> = tree<'a>
  // type a<'a> = A.t<'a>

  let rec traverse: (. (. 'a) => Applicative.t<'b>, t<'a>) => Applicative.t<t<'b>> = (. f, t) => {
    module AU = ApplicativeUtils(A)
    open AU
    switch t {
    | Leaf => pure(Leaf)
    | Node(l, x, r) => \"<*>"(\"<*>"(\"<$>"(node, traverse(f, l)), f(x)), traverse(f, r))
    }
  }
}

// From the Haskell specification we know that any traversable must be a functor. Comparing the
// signatures for map and traverse also reveals their similarities:

// val map       : ('a -> b)       -> 'a t -> 'b t
// val traverse  : ('a -> 't A.t)  -> 'a t -> ('b t) A.t

// However, embedding map in the module signature for TRAVERSABLE forces the user to define it
// manually. Would it be possible to achieve a generic implementation expressed in terms of the
// traverse function?

// It can be done by choosing a suitable Applicative where the effect does not impact the result.
// The simplest possible type forming an applicative functor is the identity type:

type id<'a> = 'a

// for which a trivial APPLICATIVE instance exist:
module IdApplicative: APPLICATIVE with type t<'a> = id<'a> = {
  type t<'a> = id<'a>
  let pure = (. x) => x
  let map = (. f) => f
  let apply = (. f) => map(f)
}

// Using IdApplicative for the effect, traverse collapses into map:
module TreeTraversableId = TreeTraversable(IdApplicative)
let map = f => TreeTraversableId.traverse(f)

// Similar to the pattern of utility modules for extending the interface with additional functions
// we may implement another module-functor TraversableFunctor that produces a functor instance given
// a module-functor for building traversables:
module TraversableFunctor = (
  MT: (A: APPLICATIVE) => (TRAVERSABLE with type Applicative.t<'a> = A.t<'a>),
) => {
  module TI = MT(IdApplicative)
  let map = (. f) => TI.traverse(f)
}

// Following is an example creating a functor for trees derived from its traversable implementation:
module TTU = TraversableFunctor(TreeTraversable)

// Its map function can be used as in:
TTU.map(x => x * x, node(Leaf, 3, node(Leaf, 5, Leaf)))->(log2("TTU.map = ", _))

// The Haskell documentation for Applicative also dictates a set of laws:
//  -- Naturality
//  t . traverse f = traverse (t . f)
//
//  -- Identity
//  traverse Identity = Identity
//
//  -- Composition
//  traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

// The naturality law assumes that t is a natural transformation from one applicative functor to
// another. Porting it to OCaml requires a couple of further tricks. First we dedicate a specific
// module functor for the task which takes two arguments for the applicatives mapped between, along
// with a traversable constructor. In order to also connect the types, an additional module TYPE2
// representing types of kind (* -> *) is introduced:

module type TYPE2 = {
  type t<'a>
}

module TestTraversableNat = (
  T2: TYPE2,
  A1: APPLICATIVE,
  A2: APPLICATIVE,
  MT: (A: APPLICATIVE) =>
  (TRAVERSABLE with type Applicative.t<'a> = A.t<'a> and type t<'a> = T2.t<'a>),
) => {
  module T1: TRAVERSABLE with type Applicative.t<'a> = A1.t<'a> and type t<'a> = T2.t<'a> = MT(A1)

  module T2: TRAVERSABLE with type Applicative.t<'a> = A2.t<'a> and type t<'a> = T2.t<'a> = MT(A2)

  type nat = {t: 'a. (. A1.t<'a>) => A2.t<'a>}

  let test = (f, {t}, x) => t(T1.traverse(f, x)) == T2.traverse(\">>"(f, t), x)
}

// Here, nat represents the mapping from A1 to A2 and the type is introduced in order to be able to
// express that the transformation is existentially quantified over all type parameters to A1.t.

// Here's an example of a concrete realization of a test module:
module TTN = TestTraversableNat(
  {
    type t<'a> = list<'a>
  },
  IdApplicative,
  OptionApplicative,
  ListTraversable,
)

// The second law, identity, is expressed in terms of the type Identity and its functor and
// applicative instances in Haskell. Identity in haskell is defined as:

// newtype Identity a = Identity a
//
// instance Functor Identity where
//   fmap f (Identity x)  = Identity (f x)
//
// instance Applicative Identity where
//   pure = Identity
//   (Identity f) <*> (Identity x) = Identity (f x)

// We've already seen its corresponding OCaml type 'a id and the applicative instance,
// IdApplicative. Using that we may create another test module for the identity law:

module TestTraversableId = (
  MT: (A: APPLICATIVE) => (TRAVERSABLE with type Applicative.t<'a> = A.t<'a>),
) => {
  module TI = MT(IdApplicative)
  let test = (. x) => TI.traverse(id, x) == x
}

// The following example shows how it can be used to test the ListTraversable module-functor:
module TTIL = TestTraversableId(ListTraversable)

{
  TTIL.test(list{1, 2, 3})->(log2("TTIL.test = ", _))
}

// The final law, composibility, relies on the type Compose which takes two higher-kinded type
// arguments and composes them:
//
// newtype Compose f g a = Compose (f (g a))
//
// Its functor and applicative functor instances are achieved by:
//
//  instance (Functor f, Functor g) => Functor (Compose f g) where
//    fmap f (Compose x) = Compose (fmap (fmap f) x)
//
//  instance (Applicative f, Applicative g) => Applicative (Compose f g) where
//    pure x = Compose (pure (pure x))
//    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

// Once again to circumvent the higher-kinded type restriction we need to resort to modules in
// OCaml. The following module-functor takes two applicatives as arguments and produces an
// APPLICATIVE module for the composed type:

module ComposeApplicative = (F: APPLICATIVE, G: APPLICATIVE): (
  APPLICATIVE with type t<'a> = F.t<G.t<'a>>
) => {
  type t<'a> = F.t<G.t<'a>>

  let pure = (. x) => F.pure(G.pure(x))

  let map = (. f) => F.map(G.map(f))

  let apply = (. f, x) => {
    module FU = ApplicativeUtils(F)
    open FU
    G.apply->\"<$>"(f)->\"<*>"(x)
  }
}

// Finally tackling the law expressed using the Compose type:
//
// traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
//
// requires some even heavier plumbing. To demonstrate that it's possible, here's an implementation:
module TestTraversableCompose = (
  T2: TYPE2,
  F: APPLICATIVE,
  G: APPLICATIVE,
  MT: (A: APPLICATIVE) =>
  (TRAVERSABLE with type Applicative.t<'a> = A.t<'a> and type t<'a> = T2.t<'a>),
) => {
  module AC: APPLICATIVE with type t<'a> = F.t<G.t<'a>> = ComposeApplicative(F, G)

  module TF: TRAVERSABLE with type Applicative.t<'a> = F.t<'a> and type t<'a> = T2.t<'a> = MT(F)

  module TG: TRAVERSABLE with type Applicative.t<'a> = G.t<'a> and type t<'a> = T2.t<'a> = MT(G)

  module TC: TRAVERSABLE with type Applicative.t<'a> = F.t<G.t<'a>> and type t<'a> = T2.t<'a> = MT(
    AC,
  )

  let test = (f, g, x) =>
    F.map(TG.traverse(g), TF.traverse(f, x)) == TC.traverse(\">>"(f, F.map(g)), x)
}

// It can be used for testing various combinations of traversables and applicatives. For example:
module TTCL = TestTraversableCompose(
  {
    type t<'a> = list<'a>
  },
  ListApplicative,
  OptionApplicative,
  ListTraversable,
)

{
  TTCL.test(
    x => list{x, x + 1},
    x =>
      if x > 10 {
        Some(-x)
      } else {
        None
      },
    list{1, 2, 3, 5},
  )->log
}

//
// Using the patterns
//
