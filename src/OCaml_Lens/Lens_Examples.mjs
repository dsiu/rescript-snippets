// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Lens from "./Lens.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var scifi_novel = {
  name: "Metro 2033",
  author: "Dmitry Glukhovsky",
  editor: {
    name: "Vitali Gubarev",
    salary: 1300,
    car: {
      make: "Lada",
      model: "VAZ-2103",
      mileage: 310000
    }
  }
};

function car_lens_get(x) {
  return x.car;
}

function car_lens_set(v, x) {
  return {
          name: x.name,
          salary: x.salary,
          car: v
        };
}

var car_lens = {
  get: car_lens_get,
  set: car_lens_set
};

function editor_lens_get(x) {
  return x.editor;
}

function editor_lens_set(v, x) {
  return {
          name: x.name,
          author: x.author,
          editor: v
        };
}

var editor_lens = {
  get: editor_lens_get,
  set: editor_lens_set
};

function mileage_lens_get(x) {
  return x.mileage;
}

function mileage_lens_set(v, x) {
  return {
          make: x.make,
          model: x.model,
          mileage: v
        };
}

var mileage_lens = {
  get: mileage_lens_get,
  set: mileage_lens_set
};

var a = Lens.compose(mileage_lens, Lens.compose(car_lens, editor_lens));

var b = Lens.compose(Lens.compose(mileage_lens, car_lens), editor_lens);

var prim1 = Lens._set(10, scifi_novel, a);

console.log("scifi with updated milage", prim1);

export {
  log ,
  log2 ,
  scifi_novel ,
  car_lens ,
  editor_lens ,
  mileage_lens ,
  a ,
  b ,
}
/* a Not a pure module */
