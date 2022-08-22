// https://cs3110.github.io/textbook/chapters/modules/modules.html#module-type-semantics
//

// We’ve already seen that OCaml will infer a signature as the type of a module. Let’s now see how
// to write those modules types ourselves. As an example, here is a module type for our list-based
// stacks:
//
// module type ModuleTypeName = sig
//  specifications
// end
//
module type LIST_STACK = {
  exception Empty
  let empty: list<'a>
  let is_empty: list<'a> => bool
  let push: ('a, list<'a>) => list<'a>
  let peek: list<'a> => 'a
  let pop: list<'a> => list<'a>
}

// Now that we have both a module and a module type for list-based stacks, we should move the
// specification comments from the structure into the signature. Those comments are properly
// part of the specification of the names in the signature. They specify behavior, thus
// augmenting the specification of types provided by the val declarations.
//

module ListStack: LIST_STACK = {
  let empty = list{}

  let is_empty = list =>
    switch list {
    | list{} => true
    | _ => false
    }

  let push = (x, list) => list{x, ...list}

  exception Empty

  let peek = list => {
    switch list {
    | list{} => raise(Empty)
    | list{x, ..._} => x
    }
  }

  let pop = list => {
    switch list {
    | list{} => raise(Empty)
    | list{_, ...s} => s
    }
  }
}

module ListStackAlias: LIST_STACK = ListStack

module M: {
  let x: int
} = {
  let x = 42
}

module type X = {
  let x: int
}

module type T = {
  module Inner: X
}

module M_: T = {
  module Inner: X = {
    let x = 42
  }
}

// https://cs3110.github.io/textbook/chapters/modules/modules.html#module-type-semantics
//
// 5.2.4. Module Type Semantics
//
// If M is just a struct block, its module type is whatever signature the compiler infers for it.
// But that can be changed by module type annotations. The key question we have to answer is: what
// does a type annotation mean for modules? That is, what does it mean when we write
// the : T in module M : T = ...?
//

// There are two properties the compiler guarantees:
//
// 1. Signature matching: every name declared in T is defined in M at the same or a more general type.
//
// 2. Opacity: any name defined in M that does not appear in T is not visible to code outside of M.
//

// Now it’s time to return to OCaml. Its module system also uses subtyping, with the same
// underlying intuition about the Liskov Substitution Principle. But OCaml uses a different flavor
// called structural subtyping. That is, it is based on the structure of modules rather than their
// names. “Structure” here simply means the definitions contained in the module. Those definitions
// are used to determine whether (M : T) is acceptable as a type annotation, where M is a module
// and T is a module type.

module M_5_2_4 = {
  module M = {
    let x = 0
    let z = 2
  }

  module type X = {
    let x: int
  }

  module MX: X = M

  module type Z = {
    let z: int
  }

  module MZ: Z = M

  module type XZ = {
    let x: int
    let z: int
  }

  module MXZ: XZ = M
}

// It’s because the types do not have to be exactly the same. If the provided value’s type is
// polymorphic, it suffices for the required value’s type to be an instantiation of that
//polymorphic type.
//
// For example, if a signature requires a type int -> int, it suffices for a structure to provide
// a value of type 'a -> 'a:
//
module type IntFun = {
  let f: int => int
}

module IdFun = {
  let f = x => x
}

module Iid: IntFun = IdFun

// After all those examples, here are the static semantics of module type annotations:
//
// - Module type annotation (M : T) is valid if the module type of M is a subtype of T.
//  The module type of (M : T) is then T in any further type checking.
//
// - Module type S is a subtype of T if the set of definitions in S is a superset of those in T.
//   Definitions in T are permitted to instantiate type variables from S.
//
// The “sub” vs. “super” in the second rule is not a typo. Consider these module types and modules:
module EX_1 = {
  module type T = {
    let a: int
  }

  module type S = {
    let a: int
    let b: bool
  }

  module A = {
    let a = 0
  }

  module AB = {
    let a = 0
    let b = true
  }

  module AC = {
    let a = 0
    let c = 'c'
  }
}

// Module type S provides a superset of the definitions in T, because it adds a definition of b.
// So why is S called a subtype of T? Think about the set Type ( T ) of all module values M
// such that M : T. That set contains A, AB, AC, and many others. Also think about
// the set Type ( S ) of all module values M such that M : S. That set contains AB but not A nor AC.
// So Type ( S ) ⊂ Type ( T ) , because there are some module values that are in Type ( T ) but not
// in Type ( S ) .

// 5.2.5. Module Types are Static
//
// Decisions about validity of module type annotations are made at compile time rather than run time.
//
// IMPORTANT:
// Module type annotations therefore offer potential confusion to programmers accustomed to
// object-oriented languages, in which subtyping works differently.
//

// 5.2.6. First-Class Modules
//
// Modules are not as first-class in OCaml as functions. But it is possible to package modules
// as first-class values. Briefly:

// (module M : T) packages module M with module type T into a value.
// (val e : T) un-packages e into a module with type T.
//
// We won’t cover this much further, but if you’re curious you can have a look at the manual.
//
