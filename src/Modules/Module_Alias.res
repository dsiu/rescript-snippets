// https://forum.rescript-lang.org/t/switching-on-js-json-parseexn/1829/6

// alias
module Option = Belt.Option

//open Belt
let x = Some(2)->Option.map(x => x * 2)

// pick some from module
let {map, keep} = module(Belt.Option)
