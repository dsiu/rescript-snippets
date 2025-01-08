//
// https://zhuanlan.zhihu.com/p/383030756
//

// 代数数据类型，英文名为 algrebraic data type (ADT), 是ML系列语言里的一个标志性特性，ReScript同样继承了这一特性，比如我们要定义一个链表结构:

type rec list =
  | Empty
  | Node({val: int, next: list})

// 它的主要表达力在于和模式匹配相结合， 比如我们先写一个简单的 length函数:
let rec length = l => {
  switch l {
  | Empty => 0
  | Node({next}) => length(next) + 1
  }
}

// 同样的很多递归函数可以很自然的写出来
let rec map = (l, f) => {
  switch l {
  | Empty => Empty
  | Node({val, next}) => Node({val: f(val), next: map(next, f)})
  }
}

let rec sum = l => {
  switch l {
  | Empty => 0
  | Node({val, next}) => val + sum(next)
  }
}

// 和其他函数式语言比如Haskell一个标志性不一样的地方是ReScript允许修改
type rec seq =
  | Empty
  | Node({mutable val: int, mutable next: seq})

// 其中 mutable 关键字表示这个链表的某些field是可以修改的。这里面的mutable 是ReScript 比较务实的主要方面，我们可以比较两种风格的append, 第一种是函数式风格的 第二种是原地操作性质的

// 函数式风格
let rec append = (l, r) => {
  switch l {
  | Empty => r
  | Node({val, next}) => Node({val, next: append(next, r)})
  }
}

// 原地操作
let rec append_ = (l: seq, r: seq) => {
  switch l {
  | Empty => ()
  | Node({next: Empty} as cell) => cell.next = r
  | Node({next}) => append_(next, r)
  }
}
