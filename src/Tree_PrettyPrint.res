open Stdlib
let log = Console.log

type rec tree = Node(string, list<tree>)

let rec print_tree: (~pad: (string, string)=?, tree) => unit = (~pad=("", ""), tree) => {
  let (pd, pc) = pad
  switch tree {
  | Node(tag, cs) => {
      `${pd}${tag}`->log
      let n = List.length(cs) - 1
      cs->List.forEachWithIndex((c, i) => {
        let pad = (pc ++ (i == n ? "`-- " : "|-- "), pc ++ (i == n ? "    " : "|   "))
        print_tree(~pad, c)
      })
    }
  }
}

let tree = Node(
  ".",
  list{Node("S", list{Node("T", list{Node("U", list{})}), Node("V", list{})}), Node("W", list{})},
)

let _ = print_tree(tree)
