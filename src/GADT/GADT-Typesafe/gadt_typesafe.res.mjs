// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

let $$default = {
  src: "/static/images/image-placeholder.jpg",
  mask: "/static/images/editor/stroke-01.svg",
  maskAngle: 0
};

function toString(prop) {
  switch (prop._0) {
    case "Src" :
      return "src";
    case "Mask" :
      return "mask";
    case "MaskAngle" :
      return "maskAngle";
  }
}

function getProperty(metadata, prop) {
  let someProp = toString({
    TAG: "SomeProperty",
    _0: prop
  });
  let v = metadata[someProp];
  if (v == null) {
    return Primitive_option.fromNullable($$default[someProp]);
  } else {
    return v;
  }
}

console.log(getProperty($$default, "Src"), "src");

console.log(getProperty($$default, "MaskAngle"), "maskAngle");

export {
  $$default as default,
  toString,
  getProperty,
}
/*  Not a pure module */
