//
// ref: https://fsharpforfunandprofit.com/posts/function-composition/
//

let log = Js.log
let compose = FP_Utils.compose

let add = (n, x) => x + n
let times = (n, x) => x * n

let add1Times2 = compose(add(1), times(2))
let add5Times3 = compose(add(5), times(3))

1->add1Times2->log
2->add5Times3->log

let twice = f => compose(f, f)

let add1 = add(1)
let add1Twice = add1->twice

9->add1Twice->log

let add1ThenMultiply = compose(add1, times)
add1ThenMultiply(2, 7)->log
