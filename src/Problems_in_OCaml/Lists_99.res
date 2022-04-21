// 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
let rec last = l => {
  switch l {
  | list{} => None
  | list{x} => Some(x)
  | list{_, ...rest} => rest->last
  }
}

// 2. Find the last but one (last and penultimate) elements of a list. (easy)
let rec last_two = l => {
  switch l {
  | list{}
  | list{_} =>
    None
  | list{_, _} as m => Some(m)
  | list{_, _, ...rest} => rest->last_two
  }
}

// 3. Find the K'th element of a list. (easy)
let rec at = (l, k) => {
  switch l {
  | list{} => None
  | list{h, ...t} => k == 1 ? Some(h) : at(t, k - 1)
  }
}

// 4. Find the number of elements of a list. (easy)
let rec length = l => {
  switch l {
  | list{} => 0
  | list{_, ...rest} => 1 + rest->length
  }
}

// 5. Reverse a list. (easy)
let rev = l => {
  let rec aux = (l', acc) => {
    switch l' {
    | list{} => acc
    | list{h, ...t} => aux(t, list{h, ...acc})
    }
  }
  aux(l, list{})
}

// 6. Find out whether a list is a palindrome. (easy)
// HINT: a palindrome is its own reverse.
let is_palindrome = l => Belt.List.eq(l, l->rev, (a, b) => a === b)

// 7. Flatten a nested list structure. (medium)
type rec node<'a> =
  | One('a)
  | Many(list<node<'a>>)

let flatten = l => {
  let rec aux = (l', acc) => {
    switch l' {
    | list{} => acc
    | list{One(x), ...t} => aux(t, list{x, ...acc})
    | list{Many(xs), ...t} => aux(t, aux(xs, acc))
    }
  }
  aux(l, list{})->rev
}

// 8. Eliminate consecutive duplicates of list elements. (medium)
let rec compress = l => {
  switch l {
  | list{a, b, ...t} => a == b ? compress(list{b, ...t}) : list{a, ...compress(list{b, ...t})}
  | _ as smaller => smaller
  }
}

// 9. Pack consecutive duplicates of list elements into sublists. (medium)
let pack = l => {
  let rec aux = (l, current, acc) => {
    switch l {
    | list{} => list{}
    | list{x} => list{list{x, ...current}, ...acc}
    | list{a, b, ...t} =>
      a == b
        ? aux(list{b, ...t}, list{a, ...current}, acc)
        : aux(list{b, ...t}, list{}, list{list{a, ...current}, ...acc})
    }
  }

  aux(l, list{}, list{})->rev
}

// 10. Run-length encoding of a list. (easy)
let encode = l => {
  let rec aux = (l, count, acc) => {
    switch l {
    | list{} => list{}
    | list{x} => list{(count + 1, x), ...acc}
    | list{a, b, ...t} =>
      a == b
        ? aux(list{b, ...t}, count + 1, acc)
        : aux(list{b, ...t}, 0, list{(count + 1, a), ...acc})
    }
  }

  aux(l, 0, list{})->rev
}
