/* depth-first-search

 "Introduction to Algorithms" - Cormen et. al., 1994

 https://blog.shaynefletcher.org/2014/05/depth-first-search.html

*/
@@warning("-3-27")

open Belt
module Str_map = Map.String
type graph = Str_map.t<list<string>>

let log = Js.log
let logList = l => l->List.toArray->log
let log2 = (x, y) => Js.log2(y, x)
let logList2 = (l, str) => l->List.toArray->log2(str)

let mapListToString = m => {
  let b = Str_map.toList(m)
  String.concat(", ", b->List.map(((x, y)) => `${x}: ${y}`))
}

let logStrMapList = (m, str) => log(`${str}: ${mapListToString(m)}`)

module type S = {
  type state
  let string_of_state: state => string
  let depth_first_search: graph => state
}

module Dfs: S = {
  type colors = White | Gray | Black

  type state = {
    d: Str_map.t<int> /* discovery time */,
    f: Str_map.t<int> /* finishing time */,
    pred: Str_map.t<string> /* predecessor */,
    color: Str_map.t<colors> /* vertex colors */,
  }

  let string_of_state = ({d, f, pred, color}) => {
    open Printf
    let bindings = (. m, fmt) => {
      let b = Str_map.toList(m)
      String.concat(", ", b->List.map(((x, y)) => sprintf(fmt, x, y)))
    }
    sprintf(
      " d = {%s}\n f = {%s}\n pred = {%s}\n",
      bindings(. d, "'%s':'%d'"),
      bindings(. f, "'%s':'%d'"),
      bindings(. pred, "'%s':'%s'"),
    )
  }

  let depth_first_search = g => {
    let node = ((t, {d, f, pred, color}), u) => {
      let rec dfs_visit = (t, u, {d, f, pred, color}) => {
        // invariant: u MUST be White
        let edge = ((t, {d, f, pred, color}), v) => {
          if color->Str_map.getExn(v) == White {
            dfs_visit(t, v, {d: d, f: f, pred: pred->Str_map.set(v, u), color: color})
          } else {
            (t, {d: d, f: f, pred: pred, color: color})
          }
        }

        let (t, {d, f, pred, color}) = {
          let t = t + 1
          List.reduce(
            g->Str_map.getExn(u),
            (
              t,
              {
                d: d->Str_map.set(u, t),
                f: f,
                pred: pred,
                color: color->Str_map.set(u, Gray),
              },
            ),
            edge,
          )
        }

        let t = t + 1
        (t, {d: d, f: f->Str_map.set(u, t), pred: pred, color: color->Str_map.set(u, Black)})
      }

      if color->Str_map.getExn(u) == White {
        dfs_visit(t, u, {d: d, f: f, pred: pred, color: color})
      } else {
        (t, {d: d, f: f, pred: pred, color: color})
      }
    }

    // v has all vertices in g
    let v = List.reduce(Str_map.toList(g), list{}, (acc, (x, _)) => list{x, ...acc})
    let initial_state = {
      d: Str_map.empty,
      f: Str_map.empty,
      pred: Str_map.empty,
      color: v->List.reduceReverse(Str_map.empty, (m, x) => m->Str_map.set(x, White)),
    }

    // visit all vertices in g by node()
    snd(List.reduceReverse(v, (0, initial_state), node))
  }
}

/* Use Ocamlraph Module */
open Graph

let () = {
  let g = List.reduceReverse(
    list{
      ("u", list{"v", "x"}),
      ("v", list{"y"}),
      ("w", list{"z", "y"}),
      ("x", list{"v"}),
      ("y", list{"x"}),
      ("z", list{"z"}),
    },
    Str_map.empty,
    (m, (x, y)) => m->Str_map.set(x, y),
  )

  //  g->logStrMapList("g")
  let s = Dfs.depth_first_search(g)
  Printf.printf("%s\n", Dfs.string_of_state(s))
}

let () = {
  module Str = {
    type t = string
    let compare = compare
    let equal = \"="
    let hash = Hashtbl.hash
  }

  //  module G = Imperative.Graph.Concrete(Str)
  module G = Imperative.Digraph.ConcreteBidirectional(Str)

  let g = G.create()
  let temp = G.create()

  List.forEach(
    list{
      ("u", list{"v", "x"}),
      ("v", list{"y"}),
      ("w", list{"z", "y"}),
      ("x", list{"v"}),
      ("y", list{"x"}),
      ("z", list{"z"}),
    },
    ((v, e)) => {
      e->List.forEach(y => G.add_edge(g, v, y))
    },
  )

  module Dfs = Traverse.Dfs(G)
  Dfs.has_cycle(g)->log

  // Dfs.iter

  let pre = v => Js.log(` pre ${G.V.label(v)}.`)
  let post = v => Js.log(`post ${G.V.label(v)}.`)

  "iter: "->Js.log
  Dfs.iter_component(~pre, ~post, g, "w")
  "prefix: "->Js.log
  Dfs.prefix_component(pre, g, "w")

  let rec visit = it => {
    let v = Dfs.get(it)
    Js.log(`visit ${G.V.label(v)}`)
    visit(Dfs.step(it))
  }

  //  try {
  //    visit(Dfs.start(g))
  //  } catch {
  //  | Exit => ()
  //  }

  // output Graphvis DOT
  module Display = {
    include G
    let vertex_name = v => V.label(v)
    let graph_attributes = _ => list{}
    let default_vertex_attributes = _ => list{}
    let vertex_attributes = _ => list{}
    let default_edge_attributes = _ => list{}
    let edge_attributes = _ => list{}
    let get_subgraph = _ => None
  }

  module Gv = Graphviz.Dot(Display)

  temp->G.add_edge("a", "b")
  temp->G.add_edge("a", "c")
  temp->G.add_edge("b", "e")
  temp->G.add_edge("a", "a")
  //  let file_ch = open_out("DFS.dot")
  //  let () = Gv.output_graph(stdout, temp)
  let () = Gv.fprint_graph(Format.str_formatter, g)
  let s = Format.flush_str_formatter()
  "s="->Js.log
  s->Js.log
}
