// https://ocaml.org/learn/tutorials/99problems.html#Working-with-lists

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

// 11. Modified run-length encoding. (easy)

type rle<'a> =
  | One('a)
  | Many(int, 'a)

let encode_11 = l => {
  let create_tuple = (cnt, elem) => cnt == 1 ? One(elem) : Many(cnt, elem)

  let rec aux = (l, count, acc) => {
    switch l {
    | list{} => list{}
    | list{x} => list{create_tuple(count + 1, x), ...acc}
    | list{a, b, ...t} =>
      a == b
        ? aux(list{b, ...t}, count + 1, acc)
        : aux(list{b, ...t}, 0, list{create_tuple(count + 1, a), ...acc})
    }
  }

  aux(l, 0, list{})->rev
}

// 12. Decode a run-length encoded list. (medium)
let decode = l => {
  let rec many = (acc, n, x) => {
    switch n {
    | 0 => acc
    | _ => many(list{x, ...acc}, n - 1, x)
    }
  }

  let rec aux = (l, acc) => {
    switch l {
    | list{} => acc
    | list{One(x), ...t} => aux(t, list{x, ...acc})
    | list{Many(n, x), ...t} => aux(t, many(acc, n, x))
    }
  }
  aux(l->rev, list{})
}

// 13. Run-length encoding of a list (direct solution). (medium)
let encode_13 = l => {
  let rle = (count, x) => {
    switch count {
    | 0 => One(x)
    | _ => Many(count + 1, x)
    }
  }
  let rec aux = (l, count, acc) => {
    switch l {
    | list{} => list{}
    | list{x} => list{rle(count, x), ...acc}
    | list{a, b, ...t} =>
      a == b
        ? aux(list{b, ...t}, count + 1, acc)
        : aux(list{b, ...t}, 0, list{rle(count, a), ...acc})
    }
  }
  aux(l, 0, list{})->rev
}

// 14. Duplicate the elements of a list. (easy)
let rec duplicate = l => {
  switch l {
  | list{} => list{}
  | list{h, ...t} => list{h, h, ...duplicate(t)}
  }
}

// 15. Replicate the elements of a list a given number of times. (medium)
let replicate = (l, n) => {
  let rec prepend = (x, n, acc) => {
    switch n {
    | 0 => acc
    | _ => prepend(x, n - 1, list{x, ...acc})
    }
  }

  let rec aux = (l, acc) => {
    switch l {
    | list{} => acc
    | list{h, ...t} => aux(t, prepend(h, n, acc))
    }
  }
  aux(l->rev, list{})
}

// 16. Drop every N'th element from a list. (medium)
let drop = (l, n) => {
  let rec aux = (l, i) => {
    switch l {
    | list{} => list{}
    | list{h, ...t} => i == n ? aux(t, 1) : list{h, ...aux(t, i + 1)}
    }
  }
  aux(l, 1)
}

// 17. Split a list into two parts; the length of the first part is given. (easy)
let split = (l, n) => {
  let rec aux = (l, i, acc) => {
    switch l {
    | list{} => (rev(acc), list{})
    | list{h, ...t} => i == 0 ? (rev(acc), l) : aux(t, i - 1, list{h, ...acc})
    }
  }
  aux(l, n, list{})
}

// 18. Extract a slice from a list. (medium)
let slice = (l, i, k) => {
  let rec take = (l, n) => {
    switch l {
    | list{} => list{}
    | list{h, ...t} => n == 0 ? list{} : list{h, ...take(t, n - 1)}
    }
  }

  let rec drop = (l, n) => {
    switch l {
    | list{} => list{}
    | list{_, ...t} => n == 0 ? l : drop(t, n - 1)
    }
  }
  l->drop(i)->take(k - i + 1)
}

// 19. Rotate a list N places to the left. (medium)
let rotate = (l, n) => {
  let len = l->length
  let n = len == 0 ? 0 : mod(mod(n, len) + len, len)
  n == 0
    ? l
    : {
        let (a, b) = split(l, n)
        Belt.List.concat(b, a)
      }
}
