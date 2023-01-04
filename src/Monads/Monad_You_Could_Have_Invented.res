open Stdlib
let log = Js.log
let log2 = Js.log2
let compose = Function.compose

let f: float => float = x => x +. 2.0
let g: float => float = x => x *. 3.0

let f': float => (float, string) = x => (x +. 2.0, "f was called.")
let g': float => (float, string) = x => (x *. 3.0, "g was called.")

// f(g(x))
let f'g': float => (float, string) = x => {
  let (y, s) = g'(x)
  let (z, t) = f'(y)
  (z, s ++ t)
}

f'g'(1.0)->log2("f'g'(1.0) = ")

// This is hard work every time we need to compose two functions and if we had to do implement this
// kind of plumbing all the way through our code it would be a pain. What we need is to define a
// higher order function to perform this plumbing for us. As the problem is that the output from g'
// can't simply be plugged into the input to f', we need to 'upgrade' f'. So we introduce a
// function, 'bind', to do this. In other words we'd like

type bind_ = (float => (float, string), (float, string)) => (float, string)

// bind must serve two purposes: it must (1) apply f' to the correct part of g' x and (2)
// concatenate the string returned by g' with the string returned by f'.

// Write the function bind.

let bind: bind_ = (f, (gx, gs)) => {
  let (fy, fs) = f'(gx)
  (fy, gs ++ fs)
}

bind(f')(g'(1.0))->log2("bind(f')(g'(1.0)) = ")
bind(g')(f'(1.0))->log2("bind(g')(f'(1.0)) = ")

compose(g', bind(f'))(1.0)->log2("compose(g', bind(f'))(1.0) = ")
compose(f', bind(g'))(1.0)->log2("compose(g', bind(f'))(1.0) = ")

// Given a pair of debuggable functions, f' and g', we can now compose them together to make a new
// debuggable function bind f' . g'. Write this composition as f'*g'. Even though the output of g'
// is incompatible with the input of f' we still have a nice easy way to concatenate their
// operations. And this suggests another question: is there an 'identity' debuggable function. The
// ordinary identity has these properties: f . id = f and id . f = f. So we're looking for a
// debuggable function, call it unit, such that unit * f = f * unit = f. Obviously we'd expect it to
// produce the empty debugging string and otherwise act a bit like the identity.

// Define unit

let unit = x => (x, "")

// The unit allows us to 'lift' any function into a debuggable one. In fact, define

let lift = (f, x) => (f(x), "")

// or more simply, lift f = unit . f. The lifted version does much the same as the original function
// and, quite reasonably, it produces the empty string as a side effect.

let lift' = Stdlib.Function.compose(f, unit)

// Show that lift f * lift g = lift (f.g)
let liftThenCompose = x => bind(lift(f))(lift(g)(x))
let composeLifted = lift(Stdlib.Function.compose(g, f))

liftThenCompose(13.0)->log2("liftThenCompose(13.0) = ")
composeLifted(13.0)->log2("composeLifted(13.0) = ")

//
// A Container: Multivalued Functions
//

// Consider the the functions sqrt and cbrt that compute the square root and cube root,
// respectively, of a real number. These are straightforward functions of type Float -> Float
// (although sqrt will thrown an exception for negative arguments, something we'll ignore).

type complex<'a> = Complex(list<'a>)

let sqrt = x => Js.Math.pow_float(~base=x, ~exp=1.0 /. 2.0)
let cbrt = x => Js.Math.pow_float(~base=x, ~exp=1.0 /. 3.0)

// Now consider a version of these functions that works with complex numbers. Every complex number,
// besides zero, has two square roots. Similarly, every non-zero complex number has three cube
// roots. So we'd like sqrt' and cbrt' to return lists of values. In other words, we'd like

let sqrt' = x => {
  list{sqrt(x), sqrt(x +. 1.0)}
}

let cbrt' = x => {
  list{cbrt(x), cbrt(x +. 1.0), cbrt(x +. 2.0)}
}

//type bindComplex_ = (
//  complex<float> => list<complex<float>>,
//  list<complex<float>>,
//) => list<complex<float>>

let bindComplex = (f, x) => {
  List.concatMany(x->List.map(f)->List.toArray)
}

let unitComplex = x => list{x}

bindComplex(sqrt', cbrt'(2.0))->List.toArray->log2("ddd")

//
// A more complex side effect: Random Numbers
//
