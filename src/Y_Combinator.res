//
// Y Combinator
//
// https://blog.shaynefletcher.org/2015/03/y-combinator.html
//
@@warning("-3-32")

/*
  There is this blog post by Caltech computer scientist, Mike Vanier. The code in Mike's article uses the Scheme
    programming language. This one uses Python.

   A Y combinator is a higher-order function which, given argument f (say,) satisfies the
   equation Yf=f(Yf). They are said to compute a "fixed point" f′ of their argument f since f′=Yf=f(Yf)=ff′.

   A Y combinator takes a function that isn't recursive and returns a version of the function
   that is. That is, Y is a function that takes f and returns f(f(f(⋯))).

   The existence of Y combinators is amazing in that it tells us that it is possible to
   write recursive functions even in a programming language that says nothing about recursion!

   The goal here is to derive a Y.

   Start with the classic recursive definition of the factorial function.
 */

let rec fact = n => {
  if n == 0 {
    1
  } else {
    n * fact(n - 1)
  }
}

/*
  We are trying to eliminate explicit recursion. To that end, factor out the recursive call and
  make it an application of a function argument.

  That's sufficient to get rid of the explicit recursion but we mean to push on in search of
  a "library" solution to this problem, that is, some general result that can be re-used.
  Next let's get this down to a function in one argument as is this way in the λ calculus.

 */

//
// https://gist.github.com/dhil/55cf406865209ab945d8ba1484ea615c

type rec fix<'a> = Fix(fix<'a> => 'a)

let fix = x => Fix(x)
let unfix = (Fix(x)) => x

let y: 'a 'b. (('a => 'b, 'a) => 'b, 'a) => 'b = f => {
  let g = (x, a) => f(unfix(x)(x), a)

  g(fix(g))
}

/* The factorial function. */
let fact = (self, n) =>
  if n == 0 {
    1
  } else {
    n * self(n - 1)
  }

/* A representation of natural numbers. */
type rec nat = Zero | Succ(nat)

let int2nat = (self, n) =>
  if n == 0 {
    Zero
  } else {
    Succ(self(n - 1))
  }

let rec string_of_nat = x =>
  switch x {
  | Zero => "Zero"
  | Succ(n) => {
      let s = string_of_nat(n)
      `Succ(${s} )`
    }
  }

let _ = {
  let result = y(fact, 6)
  j`$result\n`->Js.log

  let result = y(int2nat, 6)
  let result_str = string_of_nat(result)
  j`$result_str\n`->Js.log
}

//
// Continue reading here:
// https://homes.cs.washington.edu/~sorawee/en/blog/2017/10-05-deriving-Y.html
// https://eli.thegreenplace.net/2016/some-notes-on-the-y-combinator/
