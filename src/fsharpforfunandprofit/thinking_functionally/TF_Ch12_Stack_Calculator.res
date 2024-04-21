//
// ref: https://fsharpforfunandprofit.com/posts/stack-based-calculator/
//

@@uncurried
@@uncurried.swap

@@warning("-32")

// type
type stack = StackContents(list<float>)

// ctor
let newStack = StackContents(list{1.0, 2.0, 3.0})
let StackContents(contents) = newStack
contents->Js.log

// push function

//let push = (x, aStack) => {
//  let StackContents(contents) = aStack
//  let newContents = list{x, ...contents}
//  StackContents(newContents)
//}

// concise version of push
let push = (StackContents(contents), x) => StackContents(list{x, ...contents})

let emptyStack = StackContents(list{})

let stackWith1 = emptyStack->push(1.0)
let stackWith2 = stackWith1->push(2.0)

stackWith1->Js.log
stackWith2->Js.log

// building on top of "push"

// explicit
//let one = stack => push(1.0, stack)
//let two = stack => push(2.0, stack)

// point free
let one = push(_, 1.0)
let two = push(_, 2.0)
let three = push(_, 3.0)
let four = push(_, 4.0)
let five = push(_, 5.0)

let empty = StackContents(list{})

let stackWith1 = empty->one
let stackWith2 = stackWith1->two
let stackWith3 = stackWith2->three

let result123 = empty->one->two->three
let result321 = empty->three->two->one

result123->Js.log
result321->Js.log

// pop
let pop = (StackContents(contents)) => {
  switch contents {
  | list{top, ...rest} => {
      let newStack = StackContents(rest)
      (top, newStack)
    }

  | list{} => raise(Not_found)
  }
}

let initialStack = empty->one->two
let (popped1, poppedStack) = initialStack->pop
let (popped2, poppedStack2) = poppedStack->pop
//let _ = empty->pop

// math functions
let add_ = stack => {
  let (x, s) = stack->pop
  let (y, s2) = s->pop
  let result = x +. y
  s2->push(result)
}

let mul_ = stack => {
  let (x, s) = stack->pop
  let (y, s2) = s->pop
  let result = x *. y
  s2->push(result)
}

// refactor
let binary = (mathFn, stack) => {
  let (y, stack') = stack->pop
  let (x, stack'') = stack'->pop
  let z = mathFn(x, y)
  stack''->push(z)
}

let float_add = (x, y) => x +. y
let float_mul = (x, y) => x *. y
let float_sub = (x, y) => x -. y
let float_div = (x, y) => x /. y

let add = binary(float_add, _)
let mul = binary(float_mul, _)
let sub = binary(float_sub, _)
let div = binary(float_div, _)

let threeDivTwo = empty->three->two->div->Js.log2("threeDivTwo")
let twoSubtractFive = empty->two->five->sub->Js.log2("twoSubtractFive")
let oneAddTwoSubThree = empty->one->two->add->three->sub->Js.log2("oneAddTwoSubThree")

// unary
let unary = (. f, stack) => {
  let (x, stack') = stack->pop
  stack'->push(x->f)
}

let neg = unary(x => 0.0 -. x)
let square = unary(x => x *. x)

let neg3 = empty->three->neg->Js.log2("neg3")
let square2 = empty->two->square->Js.log2("square2")

let show = stack => {
  let (x, _) = stack->pop
  x->Js.log
  stack
}

let show2 = (stack, str) => {
  let (x, _) = stack->pop
  x->Js.log2(str)
  stack
}

let oneAddTwoSubThree = empty->one->two->add->three->sub->show2("oneAddTwoSubThree")

// helpers
let dup = stack => {
  let (x, _) = stack->pop
  stack->push(x)
}

let swap = stack => {
  let (x, s) = stack->pop
  let (y, s') = s->pop
  s'->push(x)->push(y)
}

let drop = stack => {
  let (_, s) = stack->pop
  s
}

let start = empty

// with these helpers, we can write
start->one->two->show2("1,2")->ignore
start->one->two->add->show2("1+2")->three->add->show2("+3")->ignore
start->one->two->add->show2("1+2")->three->mul->show2("*3")->two->div->show2("/2")->ignore

// using composing instead of piping
open Stdlib
let compose = Function.compose
let one_two_add = one->(@res.partial compose(two))->(compose(add, ...))
let one_two_sub = one->(@res.partial compose(two))->(compose(sub, ...))

start->one_two_add->show2("one_two_add")->ignore
start->one_two_sub->show2("one_two_sub")->ignore

let square = dup->(compose(mul, ...))
start->two->square->show2("square")->ignore

let cube = dup->(@res.partial compose(dup))->(@res.partial compose(mul))->(compose(mul, ...))
start->three->cube->show2("cube")->ignore

let compose = Function.compose
let compose3 = Function.compose3
let compose4 = Function.compose4
let sum_numbers_upto = compose(compose4(dup, one, add, mul, ...), compose(two, div, ...), ...)
start->three->square->sum_numbers_upto->show2("sum up to 9")->ignore

// pipes vs composition
// The difference is that piping is, in a sense, a “realtime transformation” operation. When you use piping you are actually doing the operations right now, passing a particular stack around.
// On the other hand, composition is a kind of “plan” for what you want to do, building an overall function from a set of parts, but not actually running it yet.

let composed_square = dup->(compose(mul, ...))
// I cannot do the equivalent with the piping approach.
// let piped_square = dup->mul

let stackWith2 = empty->two
let twoSquared = stackWith2->dup->mul

// The other way to create a “plan” is to explicitly pass in a lambda to a more primitive function, as we saw near the beginning:

let lambda_square = unary(x => x *. x)
