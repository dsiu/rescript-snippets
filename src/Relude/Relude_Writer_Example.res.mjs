// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Relude_Array from "rescript-relude/src/Relude_Array.res.mjs";
import * as Relude_WriterT from "rescript-relude/src/Relude_WriterT.res.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

let WriterLog = Relude_WriterT.WriterLog.List.WithEntry({});

let WriterList = Relude_WriterT.Writer.WithLog(WriterLog);

let map = WriterList.Infix.$less$$great;

let voidLeft = WriterList.Infix.$$great;

let flipMap = WriterList.Infix.$less$$$great;

let bind = WriterList.Infix.$great$great$eq;

let writer = bind(WriterList.pure(42), a => bind(voidLeft(Relude_WriterT.Writer.tell({
  hd: "a = " + String(a),
  tl: /* [] */0
}), (a << 1)), a => bind(voidLeft(Relude_WriterT.Writer.tell({
  hd: "a = " + String(a),
  tl: /* [] */0
}), a + 5 | 0), a => voidLeft(Relude_WriterT.Writer.tell({
  hd: "a = " + String(a),
  tl: /* [] */0
}), a))));

let match = Relude_WriterT.Writer.runWriterT(writer);

let writerLogs = match[1];

let a = match[0];

console.log(a);

let prim = Relude_Array.fromList(writerLogs);

console.log(prim);

let List;

let WriterT;

let Writer;

export {
  List,
  WriterT,
  Writer,
  log,
  log2,
  WriterLog,
  WriterList,
  map,
  voidLeft,
  flipMap,
  bind,
  writer,
  a,
  writerLogs,
}
/* WriterLog Not a pure module */
