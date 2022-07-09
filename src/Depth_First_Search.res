/* depth-first-search

 "Introduction to Algorithms" - Cormen et. al., 1994

 https://blog.shaynefletcher.org/2014/05/depth-first-search.html

*/
@@warning("-3-27")

module Char_map = Map.Make(Char)

type graph = Char_map.t<list<char>>

module type S = {
  type state
  let string_of_state: state => string
  let depth_first_search: graph => state
}

module Dfs: S = {
  type colors = White | Gray | Black

  type state = {
    d: Char_map.t<int> /* discovery time */,
    f: Char_map.t<int> /* finishing time */,
    pred: Char_map.t<char> /* predecessor */,
    color: Char_map.t<colors> /* vertex colors */,
  }

  let string_of_state = ({d, f, pred, color}) => {
    open Printf
    let bindings = (m, fmt) => {
      let b = Char_map.bindings(m)
      String.concat(", ", List.map(((x, y)) => sprintf(fmt, x, y), b))
    }
    sprintf(
      " d = {%s}\n f = {%s}\n pred = {%s}\n",
      bindings(d, "'%c':'%d'"),
      bindings(f, "'%c':'%d'"),
      bindings(pred, "'%c':'%c'"),
    )
  }

  let depth_first_search = g => {
    let node = (u, (t, {d, f, pred, color})) => {
      let rec dfs_visit = (t, u, {d, f, pred, color}) => {
        let edge = ((t, {d, f, pred, color}), v) =>
          if Char_map.find(v, color) == White {
            dfs_visit(t, v, {d: d, f: f, pred: Char_map.add(v, u, pred), color: color})
          } else {
            (t, {d: d, f: f, pred: pred, color: color})
          }

        let (t, {d, f, pred, color}) = {
          let t = t + 1
          List.fold_left(
            edge,
            (
              t,
              {
                d: Char_map.add(u, t, d),
                f: f,
                pred: pred,
                color: Char_map.add(u, Gray, color),
              },
            ),
            Char_map.find(u, g),
          )
        }

        let t = t + 1
        (t, {d: d, f: Char_map.add(u, t, f), pred: pred, color: Char_map.add(u, Black, color)})
      }

      if Char_map.find(u, color) == White {
        dfs_visit(t, u, {d: d, f: f, pred: pred, color: color})
      } else {
        (t, {d: d, f: f, pred: pred, color: color})
      }
    }

    let v = List.fold_left((acc, (x, _)) => list{x, ...acc}, list{}, Char_map.bindings(g))
    let initial_state = {
      d: Char_map.empty,
      f: Char_map.empty,
      pred: Char_map.empty,
      color: List.fold_right(x => Char_map.add(x, White), v, Char_map.empty),
    }

    snd(List.fold_right(node, v, (0, initial_state)))
  }
}

/* Test */

let () = {
  let g = List.fold_right(
    ((x, y)) => Char_map.add(x, y),
    list{
      ('u', list{'v', 'x'}),
      ('v', list{'y'}),
      ('w', list{'z', 'y'}),
      ('x', list{'v'}),
      ('y', list{'x'}),
      ('z', list{'z'}),
    },
    Char_map.empty,
  )

  let s = Dfs.depth_first_search(g)
  Printf.printf("%s\n", Dfs.string_of_state(s))
}
