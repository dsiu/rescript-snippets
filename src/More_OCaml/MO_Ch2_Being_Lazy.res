// Chapter 1
// Being Lazy

//module List = Js.List

// utils
let log = Js.log
let logList = l => l->List.toArray->log
let log2 = (x, y) => Js.log2(y, x)
let logList2 = (l, str) => l->List.toArray->log2(str)

// lazy list type
type rec lazylist<'a> = Cons('a, unit => lazylist<'a>)

// lazy number sequence
let rec lseq = n => Cons(n, () => lseq(n + 1))

// lazy utils
let lhd = (Cons(n, _)) => n
let ltl = (Cons(_, tf)) => tf()

let rec ltake = (Cons(h, tf), n) => {
  switch n {
  | 0 => list{}
  | _ => list{h, ...ltake(tf(), n - 1)}
  }
}

let rec ldrop = (Cons(_, tf) as ll, n) => {
  switch n {
  | 0 => ll
  | _ => ldrop(tf(), n - 1)
  }
}

// examples
lseq(3)->log
ltake(lseq(3), 20)->logList

// lazy map
let rec lmap = (f, Cons(h, tf)) => Cons(f(h), () => lmap(f, tf()))

// lazy filter
let rec lfilter = (f, Cons(h, tf)) => {
  f(h) ? Cons(h, () => lfilter(f, tf())) : lfilter(f, tf())
}

// using lazy utils to create cubes divisible by 5
let cubes = lfilter(x => mod(x, 5) == 0, lmap(x => x * x * x, lseq(1)))
ltake(cubes, 20)->logList

// make primes
let rec mkprimes = (Cons(h, tf)) => {
  Cons(h, () => mkprimes(lfilter(x => mod(x, h) != 0, tf())))
}

let primes = mkprimes(lseq(2))

ltake(primes, 10)->logList

// interleave
let rec interleave = (Cons(h, tf), l) => {
  Cons(h, () => interleave(l, tf()))
}

ltake(interleave(lseq(20), lseq(30)), 5)->logList

// list alternating between zeros and ones can be built with interleave
let rec lconst = n => {
  Cons(n, () => lconst(n))
}

let interleaved = {
  interleave(lconst(0), lconst(1))
}

ltake(interleaved, 9)->logList

let rec allfrom = l => {
  Cons(l, () => interleave(allfrom(list{0, ...l}), allfrom(list{1, ...l})))
}

let allones = allfrom(list{})
ltake(allones, 20)->logList

// q1 “Write the lazy list whose elements are the numbers 1, 2, 4, 8, 16…
let rec q1_ldouble = n => Cons(n, () => q1_ldouble(n * 2))
ltake(q1_ldouble(2), 10)->logList2("q1_ldouble")

// q2 “Write a function to return the nth element of a lazy list where element zero is the head of
// the list.”
let rec q2_lnth = (Cons(h, tf), n) => {
  switch n {
  | 0 => h
  | _ => q2_lnth(tf(), n - 1)
  }
}

// q3 “Write a function which, given a list, returns the lazy list forming a repeated sequence
// taken from that list. For example, given the list [1; 2; 3] it should return a lazy list with
// elements 1, 2, 3, 1, 2, 3, 1, 2…”
exception Invalid_argument(string)

let q3_lrepeating = l => {
  let rec lrepeating_inner = (c, l) => {
    switch c {
    | list{} => raise(Invalid_argument("empty list"))
    | list{x} => Cons(x, () => lrepeating_inner(l, l))
    | list{h, ...t} => Cons(h, () => lrepeating_inner(t, l))
    }
  }

  lrepeating_inner(l, l)
}

ltake(q3_lrepeating(list{2, 8, 4}), 10)->logList2("q3_lrepeating")

// q4 “Write a lazy list whose elements are the fibonacci numbers 0, 1, 1, 2, 3, 5, 8… whose first
// two elements are zero and one by definition, and each ensuing element is the sum of the
// previous two.”

let q4_fibonacci = {
  let rec fibonacci_inner = (x, y) => {
    Cons(x, () => fibonacci_inner(y, x + y))
  }
  fibonacci_inner(0, 1)
}
ltake(q4_fibonacci, 10)->logList2("q4 fibonacci")

// q5 “Write the function unleave which, given a lazy list, returns two lazy lists, one containing
// elements at positions 0, 2, 4, 6… of the original list, and the other containing elements at
// positions 1, 3, 5, 7…”
let rec q5_unleave = (Cons(h, tf)) => {
  let Cons(h', tf') = tf()
  let t = tf'()
  (Cons(h, () => fst(q5_unleave(t))), Cons(h', () => snd(q5_unleave(t))))
}

let (q5_a, q5_b) = q5_unleave(lseq(0))
ltake(q5_a, 10)->logList2("q5_unleave")
ltake(q5_b, 10)->logList2("q5_unleave")

// q6 “Alphanumeric labels in documents go A,B,C,…,X,Y,Z,AA,AB,…,BA,BB,…AAA,…  Write the
// lazy list containing strings representing this sequence.”
let rec q6_letter_string = n => {
  n <= 26
    ? Char.escaped(char_of_int(n + 64))
    : q6_letter_string((n - 1) / 26) ++ q6_letter_string(mod(n - 1, 26) + 1)
}

let alphas = lmap(q6_letter_string, lseq(1))
ltake(alphas, 100)->logList
