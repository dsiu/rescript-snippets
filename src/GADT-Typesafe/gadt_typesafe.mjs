// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";

var $$default = {
  src: "/static/images/image-placeholder.jpg",
  mask: "/static/images/editor/stroke-01.svg",
  maskAngle: 0
};

function toString(prop) {
  switch (prop._0) {
    case /* Src */0 :
        return "src";
    case /* Mask */1 :
        return "mask";
    case /* MaskAngle */2 :
        return "maskAngle";
    
  }
}

function getProperty(metadata, prop) {
  var someProp = toString(/* SomeProperty */{
        _0: prop
      });
  var v = metadata[someProp];
  if (v == null) {
    return Caml_option.nullable_to_opt($$default[someProp]);
  } else {
    return v;
  }
}

console.log(getProperty($$default, /* Src */0), "src");

console.log(getProperty($$default, /* MaskAngle */2), "maskAngle");

export {
  $$default ,
  $$default as default,
  toString ,
  getProperty ,
  
}
/*  Not a pure module */
