//
// How to implement module recursion in ReScript?
// ref:
// https://kevanstannard.github.io/rescript-blog/module-recursion.html

module type X = {
  let x: unit => int
  let y: unit => int
}

module rec A: X = {
  let x = () => 1
  let y = () => B.y() + 1
}
and B: X = {
  let x = () => A.x() + 2
  let y = () => 2
}
