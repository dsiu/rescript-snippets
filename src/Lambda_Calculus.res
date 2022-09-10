//
// https://www.youtube.com/watch?v=QuXJ3kXUCiU
//
let log = Js.log
let log2 = Js.log2

let true_ = (a, b) => a
let false_ = (a, b) => b

let not_ = b => b(false_, true_)

not_(true_)("true", "false")->log
not_(false_)("true", "false")->log
