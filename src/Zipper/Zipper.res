// The Zipper
// https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
//
@@warning("-32")

// 2.1 Trees, paths and locations
//
type rec tree<'a> =
  | Item('a)
  | Section(list<tree<'a>>)

type rec path<'a> =
  | Top
  | Node(list<tree<'a>>, path<'a>, list<tree<'a>>)
// Node(l,p,r) l: elder siblings, p: parent, r: younger siblings

// A location in the tree addresses a subtree, together with its path.
//
type location<'a> = Loc(tree<'a>, path<'a>)
//
// A location consists of a distinguished tree, the current focus of attention and its path,
// representing its surrounding context. Note that a location does not correspond to an occurrence
// in the tree, as assumed, for instance, in term rewriting theory (Huet, 1980) or in tree editors
// (Donzeau-Gouge et al., 1984). It is rather a pointer to the arc linking the designated subtree
// to the surrounding context.

// The expression a × b + c × d parses as the tree:
// [ [ a x b ] + [ c * d ] ]
//
let ex1 = {
  Section(list{
    Section(list{Item("a"), Item("*"), Item("b")}),
    Item("+"),
    Section(list{Item("c"), Item("*"), Item("d")}),
  })
}

// The location of the second multiplication sign in the tree is:
//
let loc1 = {
  Loc(
    Item("*"),
    Node(
      list{Item("c")},
      Node(list{Item("+"), Section(list{Item("a"), Item("*"), Item("b")})}, Top, list{}),
      list{Item("d")},
    ),
  )
}

// 2.2 Navigation primitives in trees
//
let go_left = (Loc(t, p)) => {
  switch p {
  | Top => failwith("left of top")
  | Node(list{l, ...left}, up, right) => Loc(l, Node(left, up, list{t, ...right}))
  | Node(list{}, up, right) => failwith("left of first")
  }
}

let go_right = (Loc(t, p)) => {
  switch p {
  | Top => failwith("right of top")
  | Node(left, up, list{r, ...right}) => Loc(r, Node(list{t, ...left}, up, right))
  | Node(left, up, list{}) => failwith("right of last")
  }
}

let go_up = (Loc(t, p)) => {
  switch p {
  | Top => failwith("up of top")
  | Node(left, up, right) => Loc(Section(List.append(List.rev(left), list{t, ...right})), up)
  }
}

let go_down = (Loc(t, p)) => {
  switch t {
  | Item(_) => failwith("down of item")
  | Section(list{t1, ...trees}) => Loc(t1, Node(list{}, p, trees))
  | _ => failwith("down of empty")
  }
}

// Note. All navigation primitives take a constant time, except go_up, which is proportional to
// the ‘juniority’ of the current term list_length(left).

// We may program with these primitives the access to the nth son of the current tree.
//
let nth = (loc, n) => {
  let rec nthrec = n => {
    switch n {
    | 1 => go_down(loc)
    | n => n > 0 ? go_right(nthrec(n - 1)) : failwith("nth expects a positive integer")
    }
  }
  nthrec(n)
}

// 2.3 Changes, insertions and deletions

let change = (Loc(_, p), t) => Loc(t, p)

// Insertion to the left or to the right is natural and cheap:
//
let insert_right = (Loc(t, p), r) => {
  switch p {
  | Top => failwith("insert of top")
  | Node(left, up, right) => Loc(t, Node(left, up, list{r, ...right}))
  }
}

let insert_left = (Loc(t, p), l) => {
  switch p {
  | Top => failwith("insert of top")
  | Node(left, up, right) => Loc(t, Node(list{l, ...left}, up, right))
  }
}

let insert_down = (Loc(t, p), t1) => {
  switch t {
  | Item(_) => failwith("down of item")
  | Section(sons) => Loc(t1, Node(list{}, p, sons))
  }
}

// We may also want to implement a deletion primitive. We may choose to move right, if possible,
// otherwise left, and up in case of an empty list.
//
let delete = (Loc(_, p)) => {
  switch p {
  | Top => failwith("delete of top")

  | Node(left, up, list{r, ...right}) => Loc(r, Node(left, up, right))
  | Node(list{l, ...left}, up, list{}) => Loc(l, Node(left, up, list{}))
  | Node(list{}, up, list{}) => Loc(Section(list{}), up)
  }
}
//
// We note that delete is not such a simple operation.
// We believe that the set of datatypes and operations above is adequate for programming the
// kernel of a structure editor in an applicative, albeit efficient, manner

// 3 Variations on the basic idea
//

// 3.1 Scars
//

// When an algorithm has frequent operations which necessitate going up in the tree, and down
// again at the same position, it is a loss of time (and space, and garbage- collecting time, etc.)
// to close the sections in the meantime. It may be advantageous to leave ‘scars’ in the structure,
// allowing direct access to the memorized visited positions. Thus, we replace the (non-empty)
// sections by triples memorizing a tree and its siblings:

type rec memo_tree<'a> =
  | Item('a)
  | Siblings(list<memo_tree<'a>>, memo_tree<'a>, list<memo_tree<'a>>)

type rec memo_path<'a> =
  | Top
  | Node(list<memo_tree<'a>>, memo_path<'a>, list<memo_tree<'a>>)

type rec memo_location<'a> = Loc(memo_tree<'a>, memo_path<'a>)

// We show the simplified up and down operations on these new structures:

let go_up_memo = (Loc(t, p)) => {
  switch p {
  | Top => failwith("up of top")
  | Node(left, p', right) => Loc(Siblings(left, t, right), p')
  }
}

let go_down_memo = (Loc(t, p)) => {
  switch t {
  | Item(_) => failwith("down of item")
  | Siblings(left, t', right) => Loc(t', Node(left, p, right))
  }
}

//  3.2 First-order terms
//

// We show, for instance, the structure corresponding to binary trees:

type rec binary_tree =
  | Nil
  | Cons(binary_tree, binary_tree)

type rec binary_path =
  | Top
  | Left(binary_path, binary_tree)
  | Right(binary_tree, binary_path)

type rec binary_location = Loc(binary_tree, binary_path)

let change = (Loc(_, p), t) => Loc(t, p)

let go_left = (Loc(t, p)) => {
  switch p {
  | Top => failwith("left of top")
  | Left(father, right) => failwith("left of Left")
  | Right(left, father) => Loc(left, Left(father, t))
  }
}

let go_right = (Loc(t, p)) => {
  switch p {
  | Top => failwith("right of top")
  | Left(father, right) => Loc(right, Right(t, father))
  | Right(left, father) => failwith("right of Right")
  }
}

let go_up = (Loc(t, p)) => {
  switch p {
  | Top => failwith("up of top")
  | Left(father, right) => Loc(Cons(t, right), father)
  | Right(left, father) => Loc(Cons(left, t), father)
  }
}

let go_first = (Loc(t, p)) => {
  switch t {
  | Nil => failwith("first of Nil")
  | Cons(left, right) => Loc(left, Left(p, right))
  }
}

let go_second = (Loc(t, p)) => {
  switch t {
  | Nil => failwith("second of Nil")
  | Cons(left, right) => Loc(right, Right(left, p))
  }
}
