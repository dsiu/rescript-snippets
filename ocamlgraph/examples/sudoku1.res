/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2007 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */

/* Ocamlgraph demo program: solving the Sudoku puzzle using graph coloring */

open Format
open Graph

/* We use undirected graphs with nodes containing a pair of integers
   (the cell coordinates in 0..8 x 0..8).
   The integer marks of the nodes will store the colors. */
module G = Imperative.Graph.Abstract({
  type t = (int, int)
})

/* The Sudoku grid = a graph with 9x9 nodes */
let g = G.create()

/* We create the 9x9 nodes, add them to the graph and keep them in a matrix
 for later access */
let nodes = {
  let new_node = (i, j) => {
    let v = G.V.create((i, j))
    G.add_vertex(g, v)
    v
  }
  Array.init(9, i => Array.init(9, new_node(i)))
}

let node = (i, j) => nodes[i][j] /* shortcut for easier access */

/* We add the edges:
   two nodes are connected whenever they can't have the same value,
   i.e. they belong to the same line, the same column or the same 3x3 group */
let () = for i in 0 to 8 {
  for j in 0 to 8 {
    for k in 0 to 8 {
      if k != i {
        G.add_edge(g, node(i, j), node(k, j))
      }
      if k != j {
        G.add_edge(g, node(i, j), node(i, k))
      }
    }
    let gi = 3 * (i / 3) and gj = 3 * (j / 3)
    for di in 0 to 2 {
      for dj in 0 to 2 {
        let i' = gi + di and j' = gj + dj
        if i' != i || j' != j {
          G.add_edge(g, node(i, j), node(i', j'))
        }
      }
    }
  }
}

/* Displaying the current state of the graph */
let display = () => {
  for i in 0 to 8 {
    for j in 0 to 8 {
      printf("%d", G.Mark.get(node(i, j)))
    }
    printf("\n")
  }
  printf("@?")
}

/* We read the initial constraints from standard input and we display g */
let () = {
  for i in 0 to 8 {
    let s = read_line()
    for j in 0 to 8 {
      switch String.get(s, j) {
      | '1' .. '9' as ch => G.Mark.set(node(i, j), Char.code(ch) - Char.code('0'))
      | _ => ()
      }
    }
  }
  display()
  printf("---------@.")
}

/* We solve the Sudoku by 9-coloring the graph g and we display the solution */
module C = Coloring.Mark(G)

let () = {
  C.coloring(g, 9)
  display()
}

/*
Local Variables:
compile-command: "make -C .. bin/sudoku.opt"
End:
*/
