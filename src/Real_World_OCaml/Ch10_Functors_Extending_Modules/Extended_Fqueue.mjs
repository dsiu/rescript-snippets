// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fqueue from "./Fqueue.mjs";
import * as Foldable from "./Foldable.mjs";

var include = Foldable.Extend({
      fold: Fqueue.fold
    });

var empty = Fqueue.empty;

var enqueue = Fqueue.enqueue;

var dequeue = Fqueue.dequeue;

var fold = Fqueue.fold;

var iter = include.iter;

var length = include.length;

var count = include.count;

var for_all = include.for_all;

var exists = include.exists;

export {
  empty ,
  enqueue ,
  dequeue ,
  fold ,
  iter ,
  length ,
  count ,
  for_all ,
  exists ,
}
/* include Not a pure module */
