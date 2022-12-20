// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function head(__x) {
  return Belt_Array.getExn(__x, 0);
}

function last(xs) {
  return Belt_Array.getExn(xs, xs.length - 1 | 0);
}

function tail(__x) {
  return Belt_Array.sliceToEnd(__x, 1);
}

function init(xs) {
  var l = xs.length;
  if (l === 0) {
    return ;
  } else {
    return Belt_Array.slice(xs, 0, l - 1 | 0);
  }
}

function uncons(xs) {
  if (xs.length !== 0) {
    return [
            Belt_Array.getExn(xs, 0),
            Belt_Array.sliceToEnd(xs, 1)
          ];
  }
  
}

function singleon(__x) {
  return Belt_Array.make(1, __x);
}

function take(xs, n) {
  var l = xs.length;
  var len = n < 0 ? 0 : (
      l < n ? l : n
    );
  return Belt_Array.slice(xs, 0, len);
}

function takeExactly(xs, n) {
  if (n < 0 || n > xs.length) {
    return ;
  } else {
    return Belt_Array.slice(xs, 0, n);
  }
}

function takeWhile(xs, predicateFn) {
  return Belt_Array.reduceU(xs, [], (function (acc, element) {
                if (Curry._1(predicateFn, element)) {
                  acc.push(element);
                }
                return acc;
              }));
}

function drop(xs, n) {
  var l = xs.length;
  var start = n < 0 ? 0 : (
      l < n ? l : n
    );
  return Belt_Array.sliceToEnd(xs, start);
}

function dropExactly(xs, n) {
  if (n < 0 || n > xs.length) {
    return ;
  } else {
    return Belt_Array.sliceToEnd(xs, n);
  }
}

function dropWhile(xs, predicateFn) {
  return Belt_Array.reduceU(xs, [], (function (acc, element) {
                if (!Curry._1(predicateFn, element)) {
                  acc.push(element);
                }
                return acc;
              }));
}

function tails(xs) {
  if (xs.length === 0) {
    return [[]];
  } else {
    return Belt_Array.concat([xs], tails(Belt_Array.sliceToEnd(xs, 1)));
  }
}

var some = Belt_Array.someU;

function uniqBy(xs, uniqFn) {
  var index = 0;
  var arr = [];
  while(index < xs.length) {
    var value = xs[index];
    var alreadyAdded = Belt_Array.someU(arr, (function(value){
        return function (x) {
          return Caml_obj.equal(Curry._1(uniqFn, x), Curry._1(uniqFn, value));
        }
        }(value)));
    if (!alreadyAdded) {
      arr.push(value);
    }
    index = index + 1 | 0;
  };
  return arr;
}

function uniq(xs) {
  return uniqBy(xs, (function (element) {
                return element;
              }));
}

function splitAt(xs, offset) {
  if (offset < 0 || offset > xs.length) {
    return ;
  } else {
    return [
            Belt_Array.slice(xs, 0, offset),
            Belt_Array.sliceToEnd(xs, offset)
          ];
  }
}

function scanl(xs, initial, fn) {
  var tmp;
  if (xs.length === 0) {
    tmp = [];
  } else {
    var h = Belt_Array.getExn(xs, 0);
    var tails = Belt_Array.sliceToEnd(xs, 1);
    tmp = scanl(tails, Curry._2(fn, initial, h), fn);
  }
  return Belt_Array.concat([initial], tmp);
}

function flatMap(xs, f) {
  return Belt_Array.reduce(Belt_Array.map(xs, f), [], Belt_Array.concat);
}

function arrayToOption(__x) {
  return Belt_Array.get(__x, 0);
}

function foldLeft(xs, f) {
  var init = Belt_Array.getExn(xs, 0);
  var rest = Belt_Array.sliceToEnd(xs, 1);
  return Belt_Array.reduce(rest, init, f);
}

function foldRight(xs, f) {
  var end = xs.length - 1 | 0;
  var init = Belt_Array.getExn(xs, end);
  var rest = Belt_Array.slice(xs, 0, end);
  return Belt_Array.reduceReverse(rest, init, f);
}

function combinationIf2(a, b, f) {
  var ret = {
    contents: []
  };
  Belt_Array.forEach(a, (function (x) {
          Belt_Array.forEach(b, (function (y) {
                  var r = f(x, y);
                  if (r !== undefined) {
                    ret.contents = Belt_Array.concat(ret.contents, [Caml_option.valFromOption(r)]);
                    return ;
                  }
                  
                }));
        }));
  return ret.contents;
}

function combination2(a, b, f) {
  return combinationIf2(a, b, (function (x, y) {
                return Caml_option.some(f(x, y));
              }));
}

function combinationIf3(a, b, c, f) {
  var ret = {
    contents: []
  };
  Belt_Array.forEach(a, (function (x) {
          Belt_Array.forEach(b, (function (y) {
                  Belt_Array.forEach(c, (function (z) {
                          var r = f(x, y, z);
                          if (r !== undefined) {
                            ret.contents = Belt_Array.concat(ret.contents, [Caml_option.valFromOption(r)]);
                            return ;
                          }
                          
                        }));
                }));
        }));
  return ret.contents;
}

function combinationArray3(a, b, c, f) {
  return combinationIf3(a, b, c, (function (x, y, z) {
                return Caml_option.some(f(x, y, z));
              }));
}

function combinationIf4(a, b, c, d, f) {
  var ret = {
    contents: []
  };
  Belt_Array.forEach(a, (function (x) {
          Belt_Array.forEach(b, (function (y) {
                  Belt_Array.forEach(c, (function (z) {
                          Belt_Array.forEach(d, (function (w) {
                                  var r = f(x, y, z, w);
                                  if (r !== undefined) {
                                    ret.contents = Belt_Array.concat(ret.contents, [Caml_option.valFromOption(r)]);
                                    return ;
                                  }
                                  
                                }));
                        }));
                }));
        }));
  return ret.contents;
}

function combination4(a, b, c, d, f) {
  return combinationIf4(a, b, c, d, (function (x, y, z, w) {
                return Caml_option.some(f(x, y, z, w));
              }));
}

var get = Belt_Array.get;

var getExn = Belt_Array.getExn;

var set = Belt_Array.set;

var setExn = Belt_Array.setExn;

var shuffleInPlace = Belt_Array.shuffleInPlace;

var shuffle = Belt_Array.shuffle;

var reverseInPlace = Belt_Array.reverseInPlace;

var reverse = Belt_Array.reverse;

var make = Belt_Array.make;

var range = Belt_Array.range;

var rangeBy = Belt_Array.rangeBy;

var makeByU = Belt_Array.makeByU;

var makeBy = Belt_Array.makeBy;

var makeByAndShuffleU = Belt_Array.makeByAndShuffleU;

var makeByAndShuffle = Belt_Array.makeByAndShuffle;

var zip = Belt_Array.zip;

var zipByU = Belt_Array.zipByU;

var zipBy = Belt_Array.zipBy;

var unzip = Belt_Array.unzip;

var concat = Belt_Array.concat;

var concatMany = Belt_Array.concatMany;

var slice = Belt_Array.slice;

var sliceToEnd = Belt_Array.sliceToEnd;

var fill = Belt_Array.fill;

var blit = Belt_Array.blit;

var blitUnsafe = Belt_Array.blitUnsafe;

var forEachU = Belt_Array.forEachU;

var forEach = Belt_Array.forEach;

var mapU = Belt_Array.mapU;

var map = Belt_Array.map;

var flatMapU = Belt_Array.flatMapU;

var getByU = Belt_Array.getByU;

var getBy = Belt_Array.getBy;

var getIndexByU = Belt_Array.getIndexByU;

var getIndexBy = Belt_Array.getIndexBy;

var keepU = Belt_Array.keepU;

var keep = Belt_Array.keep;

var keepWithIndexU = Belt_Array.keepWithIndexU;

var keepWithIndex = Belt_Array.keepWithIndex;

var keepMapU = Belt_Array.keepMapU;

var keepMap = Belt_Array.keepMap;

var forEachWithIndexU = Belt_Array.forEachWithIndexU;

var forEachWithIndex = Belt_Array.forEachWithIndex;

var mapWithIndexU = Belt_Array.mapWithIndexU;

var mapWithIndex = Belt_Array.mapWithIndex;

var partitionU = Belt_Array.partitionU;

var partition = Belt_Array.partition;

var reduceU = Belt_Array.reduceU;

var reduce = Belt_Array.reduce;

var reduceReverseU = Belt_Array.reduceReverseU;

var reduceReverse = Belt_Array.reduceReverse;

var reduceReverse2U = Belt_Array.reduceReverse2U;

var reduceReverse2 = Belt_Array.reduceReverse2;

var reduceWithIndexU = Belt_Array.reduceWithIndexU;

var reduceWithIndex = Belt_Array.reduceWithIndex;

var joinWithU = Belt_Array.joinWithU;

var joinWith = Belt_Array.joinWith;

var someU = Belt_Array.someU;

var everyU = Belt_Array.everyU;

var every = Belt_Array.every;

var every2U = Belt_Array.every2U;

var every2 = Belt_Array.every2;

var some2U = Belt_Array.some2U;

var some2 = Belt_Array.some2;

var cmpU = Belt_Array.cmpU;

var cmp = Belt_Array.cmp;

var eqU = Belt_Array.eqU;

var eq = Belt_Array.eq;

var initU = Belt_Array.initU;

var append = Belt_Array.concat;

export {
  get ,
  getExn ,
  set ,
  setExn ,
  shuffleInPlace ,
  shuffle ,
  reverseInPlace ,
  reverse ,
  make ,
  range ,
  rangeBy ,
  makeByU ,
  makeBy ,
  makeByAndShuffleU ,
  makeByAndShuffle ,
  zip ,
  zipByU ,
  zipBy ,
  unzip ,
  concat ,
  concatMany ,
  slice ,
  sliceToEnd ,
  fill ,
  blit ,
  blitUnsafe ,
  forEachU ,
  forEach ,
  mapU ,
  map ,
  flatMapU ,
  getByU ,
  getBy ,
  getIndexByU ,
  getIndexBy ,
  keepU ,
  keep ,
  keepWithIndexU ,
  keepWithIndex ,
  keepMapU ,
  keepMap ,
  forEachWithIndexU ,
  forEachWithIndex ,
  mapWithIndexU ,
  mapWithIndex ,
  partitionU ,
  partition ,
  reduceU ,
  reduce ,
  reduceReverseU ,
  reduceReverse ,
  reduceReverse2U ,
  reduceReverse2 ,
  reduceWithIndexU ,
  reduceWithIndex ,
  joinWithU ,
  joinWith ,
  someU ,
  everyU ,
  every ,
  every2U ,
  every2 ,
  some2U ,
  some2 ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  initU ,
  append ,
  head ,
  last ,
  tail ,
  init ,
  uncons ,
  singleon ,
  take ,
  takeExactly ,
  takeWhile ,
  drop ,
  dropExactly ,
  dropWhile ,
  tails ,
  some ,
  uniqBy ,
  uniq ,
  splitAt ,
  scanl ,
  flatMap ,
  arrayToOption ,
  foldLeft ,
  foldRight ,
  combinationIf2 ,
  combination2 ,
  combinationIf3 ,
  combinationArray3 ,
  combinationIf4 ,
  combination4 ,
}
/* No side effect */