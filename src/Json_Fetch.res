//
// How to fetch JSON data from Node?
//
// ref:
// https://kevanstannard.github.io/rescript-blog/fetch-json-from-node.html

// Use ad-hoc fetching:

%%raw(`
const fetch = require('node-fetch');
if (!globalThis.fetch) {
	globalThis.fetch = fetch;
}
`)

open RescriptCore

module Fetch = {
  module Response = {
    type t<'a>
    @send external json: t<'a> => Promise.t<'a> = "json"
  }
}

module Xkcd = {
  type comic = {
    img: string,
    title: string,
  }

  @val external fetch: string => Promise.t<Fetch.Response.t<comic>> = "fetch"

  let fetchCurrentComic = () =>
    fetch("http://xkcd.com/info.0.json")->Promise.then(Fetch.Response.json)
}

Xkcd.fetchCurrentComic()
->Promise.thenResolve(result => {
  Js.log(result)
})
->Promise.catch(error => {
  Js.log(error)
  Promise.resolve()
})
->ignore
