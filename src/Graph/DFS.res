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
  String.concatMany(", ", b->List.map(((x, y)) => `${x}: ${y}`)->List.toArray)
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
    let d_str = Utils.Printable.MapString.Int.toString(d)
    let f_str = Utils.Printable.MapString.Int.toString(f)
    let pred_str = Utils.Printable.MapString.String.toString(pred)
    ` d = ${d_str}\n f = ${f_str}\n pred = ${pred_str}\n`
  }

  let depth_first_search = g => {
    let node = ((t, {d, f, pred, color}), u) => {
      let rec dfs_visit = (t, u, {d, f, pred, color}) => {
        // invariant: u MUST be White
        let edge = ((t, {d, f, pred, color}), v) => {
          if color->Str_map.getExn(v) == White {
            dfs_visit(t, v, {d, f, pred: pred->Str_map.set(v, u), color})
          } else {
            (t, {d, f, pred, color})
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
                f,
                pred,
                color: color->Str_map.set(u, Gray),
              },
            ),
            edge,
          )
        }

        let t = t + 1
        (t, {d, f: f->Str_map.set(u, t), pred, color: color->Str_map.set(u, Black)})
      }

      if color->Str_map.getExn(u) == White {
        dfs_visit(t, u, {d, f, pred, color})
      } else {
        (t, {d, f, pred, color})
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
  Dfs.string_of_state(s)->log
}
