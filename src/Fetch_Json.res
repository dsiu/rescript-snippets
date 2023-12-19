// https://kevanstannard.github.io/rescript-blog/fetch-json.html

// https://aws.random.cat/meow
//
// {
//  file: "https://purr.objects-us-east-1.dream.io/i/w8V75.jpg"
//}
open RescriptCore

// for fetch() node fill-in
%%raw("require('isomorphic-fetch')")

// Fetching the data
let _ =
  Fetch.fetch("https://aws.random.cat/meow")
  ->Js.Promise.then_(Fetch.Response.json, _)
  ->Js.Promise.then_(json => Js.log(json)->Js.Promise.resolve, _)

// Declaring your API response type
type catData = {file: string}

// Converting your API response
module Decode = {
  open JSON
  let catData = (data: Js.Json.t) => {
    //    file: string("file", string, data),
    file: data
    ->Decode.object
    ->Option.flatMap(Dict.get(_, "file"))
    ->Option.flatMap(Decode.string)
    ->Option.getExn,
  }
}

// Next we’ll create a fetchCat function to perform the fetch and convert the response.
let fetchCat = () =>
  Fetch.fetch("https://aws.random.cat/meow")
  ->Js.Promise.then_(Fetch.Response.json, _)
  ->Js.Promise.then_(obj => obj->Decode.catData->Js.Promise.resolve, _)

let _ = fetchCat()->Js.Promise.then_(data => data.file->Js.log->Js.Promise.resolve, _)

// Making the fetch function more generic
let fetchJson = (url, decoder) =>
  Fetch.fetch(url)
  ->Js.Promise.then_(Fetch.Response.json, _)
  ->Js.Promise.then_(obj => obj->decoder->Js.Promise.resolve, _)

let fetchCat = () => fetchJson("https://aws.random.cat/meow", Decode.catData)

let _ = fetchCat()->Js.Promise.then_(data => data.file->Js.log->Js.Promise.resolve, _)
