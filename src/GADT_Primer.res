//
// https://sketch.sh/s/yH0MJiujNSiofDWOU85loX/
//

// ADTs as GADTs

type rec bool' =
  | True: bool'
  | False: bool'

type rec option'<'a> =
  | None': option'<'a>
  | Some'('a): option'<'a>

let mapOption' = (f, opt) =>
  switch opt {
  | None' => None'
  | Some'(x) => Some'(f(x))
  }

let a = Some'(5)
let b = None'
let inc = x => x + 1
let c = a |> mapOption'(inc)
let d = b |> mapOption'(inc)

// Unleashing GADTs
type rec prim = [
  | #Int(int)
  | #Float(float)
  | #Bool(bool)
  | #Str(string)
]

let eval = a =>
  switch a {
  | #Int(i) => i
  | #Float(f) => f
  | #Bool(b) => b
  | #Str(s) => s
  }

let myInt = eval(#Int(42))
let myFloat = eval(#Float(4.2))
let myBool = eval(#Bool(false))
let myStr = eval(#Str("Hello"))

// Heterogeneous List
