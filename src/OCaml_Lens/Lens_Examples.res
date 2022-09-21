//
// https://github.com/pdonadeo/ocaml-lens
//

open Lens
let log = Js.log
let log2 = Js.log2

type car = {
  make: string,
  model: string,
  mileage: int,
}

type editor = {
  name: string,
  salary: int,
  car: car,
}

type book = {
  name: string,
  author: string,
  editor: editor,
}

// Create a new nested record
let scifi_novel = {
  name: "Metro 2033",
  author: "Dmitry Glukhovsky",
  editor: {
    name: "Vitali Gubarev",
    salary: 1300,
    car: {
      make: "Lada",
      model: "VAZ-2103",
      mileage: 310000,
    },
  },
}

// Now to construct a few lenses to access some things
let car_lens = {
  get: x => x.car,
  set: (v, x) => {...x, car: v},
}

let editor_lens = {
  get: x => x.editor,
  set: (v, x) => {...x, editor: v},
}
let mileage_lens = {
  get: x => x.mileage,
  set: (v, x) => {...x, mileage: v},
}

// Using these lenses we can modify the mileage without having to unpack the record

let a = compose(mileage_lens, compose(car_lens, editor_lens))
let b = mileage_lens->compose(car_lens)->compose(editor_lens)
{
  _set(10, scifi_novel, a)->log2("scifi with updated milage", _)
}
