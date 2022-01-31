// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function length(l) {
  if (l) {
    return length(l.next) + 1 | 0;
  } else {
    return 0;
  }
}

console.log(length(/* Node */{
          val: 3,
          next: /* Empty */0
        }));

function len(l) {
  if (typeof l === "object") {
    return len(l.VAL[1]) + 1 | 0;
  } else {
    return 0;
  }
}

console.log(len({
          NAME: "Node",
          VAL: [
            3,
            "Empty"
          ]
        }));

function $$eval(e) {
  switch (e.TAG | 0) {
    case /* Number */0 :
        return e._0;
    case /* Add */1 :
        return $$eval(e._0) + $$eval(e._1) | 0;
    case /* Mul */2 :
        return Math.imul($$eval(e._0), $$eval(e._1));
    
  }
}

function eval_(e) {
  var variant = e.NAME;
  if (variant === "Add") {
    var match = e.VAL;
    return eval_(match[0]) + eval_(match[1]) | 0;
  }
  if (variant !== "Mul") {
    return e.VAL;
  }
  var match$1 = e.VAL;
  return Math.imul(eval_(match$1[0]), eval_(match$1[1]));
}

function httpStatus(code) {
  
}

var statusString = "Yes";

function colorToString(color) {
  if (color === "red") {
    return "Red";
  } else if (color === "green") {
    return "Green";
  } else {
    return "Blue";
  }
}

function toString(color) {
  return color;
}

var hello = "hello";

var hello2 = "hello";

var a = "hello";

var status = "yes";

var red = "red";

exports.hello = hello;
exports.hello2 = hello2;
exports.a = a;
exports.length = length;
exports.len = len;
exports.$$eval = $$eval;
exports.eval_ = eval_;
exports.httpStatus = httpStatus;
exports.status = status;
exports.statusString = statusString;
exports.colorToString = colorToString;
exports.toString = toString;
exports.red = red;
/*  Not a pure module */
