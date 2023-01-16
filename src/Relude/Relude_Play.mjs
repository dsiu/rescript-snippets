// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Relude_Int from "relude/src/Relude_Int.mjs";
import * as Relude_Tuple from "relude/src/Relude_Tuple.mjs";
import * as Relude_String from "relude/src/Relude_String.mjs";
import * as Relude_Tuple2 from "relude/src/Relude_Tuple2.mjs";
import * as Relude_ReaderT from "relude/src/Relude_ReaderT.mjs";
import * as Relude_Array_Base from "relude/src/array/Relude_Array_Base.mjs";
import * as Relude_Array_Instances from "relude/src/array/Relude_Array_Instances.mjs";
import * as Relude_Array_Specializations from "relude/src/array/Relude_Array_Specializations.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var testEnv = {
  intValue: 42,
  stringValue: "abc"
};

var Reader = Relude_ReaderT.Reader.WithEnv({});

var r = Curry._2(Reader.runReaderT, testEnv, Curry._1(Reader.make, (function (r) {
            return (r.intValue << 1);
          })));

console.log(r);

var partial_arg = Relude_Int.Ord;

var partial_arg$1 = Relude_Tuple2.WithOrds;

var $$let = (function (param) {
      return partial_arg$1(partial_arg, param);
    })(Relude_String.Ord);

var include = Relude_Array_Specializations.ArrayOrdExtensions({
      eq: $$let.eq,
      compare: $$let.compare
    });

var sort = include.sort;

var AT_StrInt_contains = include.contains;

var AT_StrInt_indexOf = include.indexOf;

var AT_StrInt_distinct = include.distinct;

var AT_StrInt_removeFirst = include.removeFirst;

var AT_StrInt_removeEach = include.removeEach;

var AT_StrInt_eq = include.eq;

var AT_StrInt_min = include.min;

var AT_StrInt_max = include.max;

var AT_StrInt = {
  concat: Relude_Array_Instances.concat,
  SemigroupAny: Relude_Array_Instances.SemigroupAny,
  concatNamed: Relude_Array_Instances.concatNamed,
  map: Relude_Array_Instances.map,
  Functor: Relude_Array_Instances.Functor,
  BsFunctorExtensions: Relude_Array_Instances.BsFunctorExtensions,
  flipMap: Relude_Array_Instances.flipMap,
  $$void: Relude_Array_Instances.$$void,
  voidRight: Relude_Array_Instances.voidRight,
  voidLeft: Relude_Array_Instances.voidLeft,
  flap: Relude_Array_Instances.flap,
  apply: Relude_Array_Instances.apply,
  Apply: Relude_Array_Instances.Apply,
  BsApplyExtensions: Relude_Array_Instances.BsApplyExtensions,
  applyFirst: Relude_Array_Instances.applyFirst,
  applySecond: Relude_Array_Instances.applySecond,
  map2: Relude_Array_Instances.map2,
  map3: Relude_Array_Instances.map3,
  map4: Relude_Array_Instances.map4,
  map5: Relude_Array_Instances.map5,
  tuple2: Relude_Array_Instances.tuple2,
  tuple3: Relude_Array_Instances.tuple3,
  tuple4: Relude_Array_Instances.tuple4,
  tuple5: Relude_Array_Instances.tuple5,
  mapTuple2: Relude_Array_Instances.mapTuple2,
  mapTuple3: Relude_Array_Instances.mapTuple3,
  mapTuple4: Relude_Array_Instances.mapTuple4,
  mapTuple5: Relude_Array_Instances.mapTuple5,
  pure: Relude_Array_Instances.pure,
  Applicative: Relude_Array_Instances.Applicative,
  BsApplicativeExtensions: Relude_Array_Instances.BsApplicativeExtensions,
  liftA1: Relude_Array_Instances.liftA1,
  bind: Relude_Array_Instances.bind,
  Monad: Relude_Array_Instances.Monad,
  BsMonadExtensions: Relude_Array_Instances.BsMonadExtensions,
  flatMap: Relude_Array_Instances.flatMap,
  flatten: Relude_Array_Instances.flatten,
  composeKleisli: Relude_Array_Instances.composeKleisli,
  flipComposeKleisli: Relude_Array_Instances.flipComposeKleisli,
  liftM1: Relude_Array_Instances.liftM1,
  when_: Relude_Array_Instances.when_,
  unless: Relude_Array_Instances.unless,
  alt: Relude_Array_Instances.alt,
  Alt: Relude_Array_Instances.Alt,
  orElse: Relude_Array_Instances.orElse,
  imap: Relude_Array_Instances.imap,
  Invariant: Relude_Array_Instances.Invariant,
  extend: Relude_Array_Instances.extend,
  Extend: Relude_Array_Instances.Extend,
  foldLeft: Relude_Array_Instances.foldLeft,
  foldRight: Relude_Array_Instances.foldRight,
  Foldable: Relude_Array_Instances.Foldable,
  BsFoldableExtensions: Relude_Array_Instances.BsFoldableExtensions,
  any: Relude_Array_Instances.any,
  all: Relude_Array_Instances.all,
  containsBy: Relude_Array_Instances.containsBy,
  indexOfBy: Relude_Array_Instances.indexOfBy,
  minBy: Relude_Array_Instances.minBy,
  maxBy: Relude_Array_Instances.maxBy,
  countBy: Relude_Array_Instances.countBy,
  size: Relude_Array_Instances.size,
  count: Relude_Array_Instances.count,
  forEach: Relude_Array_Instances.forEach,
  forEachWithIndex: Relude_Array_Instances.forEachWithIndex,
  find: Relude_Array_Instances.find,
  findWithIndex: Relude_Array_Instances.findWithIndex,
  toArray: Relude_Array_Instances.toArray,
  FoldableSemigroupExtensions: Relude_Array_Instances.FoldableSemigroupExtensions,
  FoldableMonoidExtensions: Relude_Array_Instances.FoldableMonoidExtensions,
  foldMap: Relude_Array_Instances.foldMap,
  foldWithMonoid: Relude_Array_Instances.foldWithMonoid,
  intercalate: Relude_Array_Instances.intercalate,
  FoldableApplicativeExtensions: Relude_Array_Instances.FoldableApplicativeExtensions,
  FoldableMonadExtensions: Relude_Array_Instances.FoldableMonadExtensions,
  FoldableEqExtensions: Relude_Array_Instances.FoldableEqExtensions,
  FoldableOrdExtensions: Relude_Array_Instances.FoldableOrdExtensions,
  Traversable: Relude_Array_Instances.Traversable,
  eqBy: Relude_Array_Instances.eqBy,
  Eq: Relude_Array_Instances.Eq,
  Ord: Relude_Array_Instances.Ord,
  showBy: Relude_Array_Instances.showBy,
  show: Relude_Array_Instances.show,
  Show: Relude_Array_Instances.Show,
  fromList: Relude_Array_Instances.fromList,
  toList: Relude_Array_Instances.toList,
  IsoList: Relude_Array_Instances.IsoList,
  cons: Relude_Array_Base.cons,
  prepend: Relude_Array_Base.prepend,
  uncons: Relude_Array_Base.uncons,
  append: Relude_Array_Base.append,
  repeat: Relude_Array_Base.repeat,
  makeWithIndex: Relude_Array_Base.makeWithIndex,
  mapWithIndex: Relude_Array_Base.mapWithIndex,
  reverse: Relude_Array_Base.reverse,
  shuffleInPlace: Relude_Array_Base.shuffleInPlace,
  shuffle: Relude_Array_Base.shuffle,
  length: Relude_Array_Base.length,
  isEmpty: Relude_Array_Base.isEmpty,
  isNotEmpty: Relude_Array_Base.isNotEmpty,
  at: Relude_Array_Base.at,
  setAt: Relude_Array_Base.setAt,
  head: Relude_Array_Base.head,
  tail: Relude_Array_Base.tail,
  tailOrEmpty: Relude_Array_Base.tailOrEmpty,
  init: Relude_Array_Base.init,
  initOrEmpty: Relude_Array_Base.initOrEmpty,
  last: Relude_Array_Base.last,
  take: Relude_Array_Base.take,
  takeExactly: Relude_Array_Base.takeExactly,
  takeWhile: Relude_Array_Base.takeWhile,
  drop: Relude_Array_Base.drop,
  dropExactly: Relude_Array_Base.dropExactly,
  dropWhile: Relude_Array_Base.dropWhile,
  filter: Relude_Array_Base.filter,
  keep: Relude_Array_Base.keep,
  filterWithIndex: Relude_Array_Base.filterWithIndex,
  keepWithIndex: Relude_Array_Base.keepWithIndex,
  filterNot: Relude_Array_Base.filterNot,
  reject: Relude_Array_Base.reject,
  filterNotWithIndex: Relude_Array_Base.filterNotWithIndex,
  rejectWithIndex: Relude_Array_Base.rejectWithIndex,
  mapOption: Relude_Array_Base.mapOption,
  catOption: Relude_Array_Base.catOption,
  partition: Relude_Array_Base.partition,
  splitAt: Relude_Array_Base.splitAt,
  prependToAll: Relude_Array_Base.prependToAll,
  intersperse: Relude_Array_Base.intersperse,
  replicate: Relude_Array_Base.replicate,
  zip: Relude_Array_Base.zip,
  zipWith: Relude_Array_Base.zipWith,
  zipWithIndex: Relude_Array_Base.zipWithIndex,
  unzip: Relude_Array_Base.unzip,
  sortWithInt: Relude_Array_Base.sortWithInt,
  sortBy: Relude_Array_Base.sortBy,
  distinctBy: Relude_Array_Base.distinctBy,
  removeFirstBy: Relude_Array_Base.removeFirstBy,
  removeEachBy: Relude_Array_Base.removeEachBy,
  replaceAt: Relude_Array_Base.replaceAt,
  scanLeft: Relude_Array_Base.scanLeft,
  scanRight: Relude_Array_Base.scanRight,
  insertAt: Relude_Array_Base.insertAt,
  updateAt: Relude_Array_Base.updateAt,
  swapAt: Relude_Array_Base.swapAt,
  removeAt: Relude_Array_Base.removeAt,
  chunk: Relude_Array_Base.chunk,
  contains: AT_StrInt_contains,
  indexOf: AT_StrInt_indexOf,
  distinct: AT_StrInt_distinct,
  removeFirst: AT_StrInt_removeFirst,
  removeEach: AT_StrInt_removeEach,
  eq: AT_StrInt_eq,
  min: AT_StrInt_min,
  max: AT_StrInt_max,
  sort: sort
};

var arr = [
  [
    3,
    "c"
  ],
  [
    26,
    "z"
  ],
  [
    1,
    "a"
  ],
  [
    2,
    "b"
  ]
];

var sorted = Curry._1(sort, arr);

console.log(arr);

console.log(sorted);

function plus1(param) {
  var s = param[1];
  return Relude_Array_Instances.pure([
              param[0] + 1 | 0,
              s + s
            ]);
}

function reverseTuple(param) {
  return Relude_Array_Instances.pure([
              param[1],
              param[0]
            ]);
}

var prim = Curry._2(Relude_Array_Instances.bind, arr, plus1);

console.log(prim);

var arr_rev = Curry._2(Relude_Array_Instances.bind, arr, reverseTuple);

var partial_arg$2 = Relude_String.Ord;

var partial_arg$3 = Relude_Tuple2.WithOrds;

var $$let$1 = (function (param) {
      return partial_arg$3(partial_arg$2, param);
    })(Relude_Int.Ord);

var include$1 = Relude_Array_Specializations.ArrayOrdExtensions({
      eq: $$let$1.eq,
      compare: $$let$1.compare
    });

var sort$1 = include$1.sort;

var AT_IntStr_contains = include$1.contains;

var AT_IntStr_indexOf = include$1.indexOf;

var AT_IntStr_distinct = include$1.distinct;

var AT_IntStr_removeFirst = include$1.removeFirst;

var AT_IntStr_removeEach = include$1.removeEach;

var AT_IntStr_eq = include$1.eq;

var AT_IntStr_min = include$1.min;

var AT_IntStr_max = include$1.max;

var AT_IntStr = {
  concat: Relude_Array_Instances.concat,
  SemigroupAny: Relude_Array_Instances.SemigroupAny,
  concatNamed: Relude_Array_Instances.concatNamed,
  map: Relude_Array_Instances.map,
  Functor: Relude_Array_Instances.Functor,
  BsFunctorExtensions: Relude_Array_Instances.BsFunctorExtensions,
  flipMap: Relude_Array_Instances.flipMap,
  $$void: Relude_Array_Instances.$$void,
  voidRight: Relude_Array_Instances.voidRight,
  voidLeft: Relude_Array_Instances.voidLeft,
  flap: Relude_Array_Instances.flap,
  apply: Relude_Array_Instances.apply,
  Apply: Relude_Array_Instances.Apply,
  BsApplyExtensions: Relude_Array_Instances.BsApplyExtensions,
  applyFirst: Relude_Array_Instances.applyFirst,
  applySecond: Relude_Array_Instances.applySecond,
  map2: Relude_Array_Instances.map2,
  map3: Relude_Array_Instances.map3,
  map4: Relude_Array_Instances.map4,
  map5: Relude_Array_Instances.map5,
  tuple2: Relude_Array_Instances.tuple2,
  tuple3: Relude_Array_Instances.tuple3,
  tuple4: Relude_Array_Instances.tuple4,
  tuple5: Relude_Array_Instances.tuple5,
  mapTuple2: Relude_Array_Instances.mapTuple2,
  mapTuple3: Relude_Array_Instances.mapTuple3,
  mapTuple4: Relude_Array_Instances.mapTuple4,
  mapTuple5: Relude_Array_Instances.mapTuple5,
  pure: Relude_Array_Instances.pure,
  Applicative: Relude_Array_Instances.Applicative,
  BsApplicativeExtensions: Relude_Array_Instances.BsApplicativeExtensions,
  liftA1: Relude_Array_Instances.liftA1,
  bind: Relude_Array_Instances.bind,
  Monad: Relude_Array_Instances.Monad,
  BsMonadExtensions: Relude_Array_Instances.BsMonadExtensions,
  flatMap: Relude_Array_Instances.flatMap,
  flatten: Relude_Array_Instances.flatten,
  composeKleisli: Relude_Array_Instances.composeKleisli,
  flipComposeKleisli: Relude_Array_Instances.flipComposeKleisli,
  liftM1: Relude_Array_Instances.liftM1,
  when_: Relude_Array_Instances.when_,
  unless: Relude_Array_Instances.unless,
  alt: Relude_Array_Instances.alt,
  Alt: Relude_Array_Instances.Alt,
  orElse: Relude_Array_Instances.orElse,
  imap: Relude_Array_Instances.imap,
  Invariant: Relude_Array_Instances.Invariant,
  extend: Relude_Array_Instances.extend,
  Extend: Relude_Array_Instances.Extend,
  foldLeft: Relude_Array_Instances.foldLeft,
  foldRight: Relude_Array_Instances.foldRight,
  Foldable: Relude_Array_Instances.Foldable,
  BsFoldableExtensions: Relude_Array_Instances.BsFoldableExtensions,
  any: Relude_Array_Instances.any,
  all: Relude_Array_Instances.all,
  containsBy: Relude_Array_Instances.containsBy,
  indexOfBy: Relude_Array_Instances.indexOfBy,
  minBy: Relude_Array_Instances.minBy,
  maxBy: Relude_Array_Instances.maxBy,
  countBy: Relude_Array_Instances.countBy,
  size: Relude_Array_Instances.size,
  count: Relude_Array_Instances.count,
  forEach: Relude_Array_Instances.forEach,
  forEachWithIndex: Relude_Array_Instances.forEachWithIndex,
  find: Relude_Array_Instances.find,
  findWithIndex: Relude_Array_Instances.findWithIndex,
  toArray: Relude_Array_Instances.toArray,
  FoldableSemigroupExtensions: Relude_Array_Instances.FoldableSemigroupExtensions,
  FoldableMonoidExtensions: Relude_Array_Instances.FoldableMonoidExtensions,
  foldMap: Relude_Array_Instances.foldMap,
  foldWithMonoid: Relude_Array_Instances.foldWithMonoid,
  intercalate: Relude_Array_Instances.intercalate,
  FoldableApplicativeExtensions: Relude_Array_Instances.FoldableApplicativeExtensions,
  FoldableMonadExtensions: Relude_Array_Instances.FoldableMonadExtensions,
  FoldableEqExtensions: Relude_Array_Instances.FoldableEqExtensions,
  FoldableOrdExtensions: Relude_Array_Instances.FoldableOrdExtensions,
  Traversable: Relude_Array_Instances.Traversable,
  eqBy: Relude_Array_Instances.eqBy,
  Eq: Relude_Array_Instances.Eq,
  Ord: Relude_Array_Instances.Ord,
  showBy: Relude_Array_Instances.showBy,
  show: Relude_Array_Instances.show,
  Show: Relude_Array_Instances.Show,
  fromList: Relude_Array_Instances.fromList,
  toList: Relude_Array_Instances.toList,
  IsoList: Relude_Array_Instances.IsoList,
  cons: Relude_Array_Base.cons,
  prepend: Relude_Array_Base.prepend,
  uncons: Relude_Array_Base.uncons,
  append: Relude_Array_Base.append,
  repeat: Relude_Array_Base.repeat,
  makeWithIndex: Relude_Array_Base.makeWithIndex,
  mapWithIndex: Relude_Array_Base.mapWithIndex,
  reverse: Relude_Array_Base.reverse,
  shuffleInPlace: Relude_Array_Base.shuffleInPlace,
  shuffle: Relude_Array_Base.shuffle,
  length: Relude_Array_Base.length,
  isEmpty: Relude_Array_Base.isEmpty,
  isNotEmpty: Relude_Array_Base.isNotEmpty,
  at: Relude_Array_Base.at,
  setAt: Relude_Array_Base.setAt,
  head: Relude_Array_Base.head,
  tail: Relude_Array_Base.tail,
  tailOrEmpty: Relude_Array_Base.tailOrEmpty,
  init: Relude_Array_Base.init,
  initOrEmpty: Relude_Array_Base.initOrEmpty,
  last: Relude_Array_Base.last,
  take: Relude_Array_Base.take,
  takeExactly: Relude_Array_Base.takeExactly,
  takeWhile: Relude_Array_Base.takeWhile,
  drop: Relude_Array_Base.drop,
  dropExactly: Relude_Array_Base.dropExactly,
  dropWhile: Relude_Array_Base.dropWhile,
  filter: Relude_Array_Base.filter,
  keep: Relude_Array_Base.keep,
  filterWithIndex: Relude_Array_Base.filterWithIndex,
  keepWithIndex: Relude_Array_Base.keepWithIndex,
  filterNot: Relude_Array_Base.filterNot,
  reject: Relude_Array_Base.reject,
  filterNotWithIndex: Relude_Array_Base.filterNotWithIndex,
  rejectWithIndex: Relude_Array_Base.rejectWithIndex,
  mapOption: Relude_Array_Base.mapOption,
  catOption: Relude_Array_Base.catOption,
  partition: Relude_Array_Base.partition,
  splitAt: Relude_Array_Base.splitAt,
  prependToAll: Relude_Array_Base.prependToAll,
  intersperse: Relude_Array_Base.intersperse,
  replicate: Relude_Array_Base.replicate,
  zip: Relude_Array_Base.zip,
  zipWith: Relude_Array_Base.zipWith,
  zipWithIndex: Relude_Array_Base.zipWithIndex,
  unzip: Relude_Array_Base.unzip,
  sortWithInt: Relude_Array_Base.sortWithInt,
  sortBy: Relude_Array_Base.sortBy,
  distinctBy: Relude_Array_Base.distinctBy,
  removeFirstBy: Relude_Array_Base.removeFirstBy,
  removeEachBy: Relude_Array_Base.removeEachBy,
  replaceAt: Relude_Array_Base.replaceAt,
  scanLeft: Relude_Array_Base.scanLeft,
  scanRight: Relude_Array_Base.scanRight,
  insertAt: Relude_Array_Base.insertAt,
  updateAt: Relude_Array_Base.updateAt,
  swapAt: Relude_Array_Base.swapAt,
  removeAt: Relude_Array_Base.removeAt,
  chunk: Relude_Array_Base.chunk,
  contains: AT_IntStr_contains,
  indexOf: AT_IntStr_indexOf,
  distinct: AT_IntStr_distinct,
  removeFirst: AT_IntStr_removeFirst,
  removeEach: AT_IntStr_removeEach,
  eq: AT_IntStr_eq,
  min: AT_IntStr_min,
  max: AT_IntStr_max,
  sort: sort$1
};

var prim$1 = Curry._1(sort$1, arr_rev);

console.log(prim$1);

var prim$2 = Relude_Array_Instances.showBy((function (param) {
        return Relude_Tuple.showBy2(Relude_String.show, Relude_Int.show, param);
      }), arr_rev);

console.log(prim$2);

var ReaderT;

var Tuple;

var $$Array;

var $$String;

var Int;

export {
  log ,
  log2 ,
  testEnv ,
  ReaderT ,
  Reader ,
  r ,
  Tuple ,
  $$Array ,
  $$String ,
  Int ,
  AT_StrInt ,
  arr ,
  sorted ,
  plus1 ,
  reverseTuple ,
  arr_rev ,
  AT_IntStr ,
}
/* Reader Not a pure module */
