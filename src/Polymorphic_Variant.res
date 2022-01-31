// ref:
// https://zhuanlan.zhihu.com/p/384308761
// 结构化的代数数据类型以#作为标识符号

let hello = #hello
let hello2 = "hello"

/*
这里面虽然hello, hello2运行时候的表示是一样的， 都是字符串，但是hello 的类型是 #hello, hello2的类型是string.

hello有着更细致的类型，我们也可以强制转换，编译器会检查并确认这种强制转换是正确的，从而允许这种类型转换。
*/
let a = (#hello: [#hello] :> string)

// 结构化的数据类型同样可以携带payload,可以模式匹配，我们可以与之前的代数数据类型 做为对比：
type rec list =
  | Empty
  | Node({val: int, next: list})

let rec length = l => {
  switch l {
  | Empty => 0
  | Node({next}) => length(next) + 1
  }
}

length(Node({val: 3, next: Empty}))->Js.log

let rec len = l => {
  switch l {
  | #Empty => 0
  | #Node(_, next) => len(next) + 1
  }
}

len(#Node(3, #Empty))->Js.log

// 可以看到结构化的数据类型不需要类型声明，编译器会根据类型的使用进行推断， 其实用起来更加简单，灵活。

// 我们之前讲到可以用十行代码实现一个简单的解释器，使用 结构化数据类型会更加简单：

type rec exp =
  | Number(int)
  | Add(exp, exp)
  | Mul(exp, exp)

let rec eval = e => {
  switch e {
  | Number(i) => i
  | Add(l, r) => eval(l) + eval(r)
  | Mul(l, r) => eval(l) * eval(r)
  }
}

let rec eval_ = e => {
  switch e {
  | #Number(i) => i
  | #Add(l, r) => eval_(l) + eval_(r)
  | #Mul(l, r) => eval_(l) * eval_(r)
  }
}

// 结构化的代数数据类型同样可以用于整数枚举之中:
// 函数只接受这 #404 和 #202两种状态
let httpStatus = (code: [#404 | #201]) => {
  code->ignore
}

/*
结构化代数数据类型的缺陷

如上所述，结构化的代数数据类型似乎非常灵活，且兼具代数数据类型的有点。

其缺点也恰恰是来自于其优点，由于过于依赖编译器的类型推导，有时候推导出来的类型会过于复杂，出错信息比较难于理解。

一般对于复杂的结构化代数数据类型，初期建模结束后会调整为代数数据类型，便于理解。
*/

// -------------------------------------------------------------------------------------------
// ref:
// https://kevanstannard.github.io/rescript-blog/polymorphic-variants.html
//

// Polymorphic variants are distinguished from ordinary variants by the leading hash.
// Unlike ordinary variants, polymorphic variants can be used without an explicit type declaration.
let status = #yes

let statusString = {
  switch status {
  | #yes => "Yes"
  | #no => "No"
  }
}

// However they may be assigned to a type:
type color = [#red | #green | #blue]

let colorToString = (color: color): string => {
  switch color {
  | #red => "Red"
  | #green => "Green"
  | #blue => "Blue"
  }
}

// How to convert a polymorphic variable to a string?
// ref:
// https://kevanstannard.github.io/rescript-blog/convert-polyvar-to-string.html

// Coerce the polyvar to a string
let toString = (color: color): string => (color :> string)

let red: string = toString(#red)
