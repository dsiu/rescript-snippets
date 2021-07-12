// https://forum.rescript-lang.org/t/switching-on-js-json-parseexn/1829

// v1
let safeParse1 = string =>
  try {
    let json = Js.Json.parseExn(string)
    Belt.Result.Ok(json)
  } catch {
  | Js.Exn.Error(obj) =>
    switch Js.Exn.message(obj) {
    | Some(m) => Belt.Result.Error(m)
    | None => Belt.Result.Error("Unknown")
    }
  }

// v2
let safeParse = string =>
  switch Js.Json.parseExn(string) {
  | json => Ok(json)
  | exception Js.Exn.Error(exn) => exn->Js.Exn.message->Belt.Option.getWithDefault("Unknown")->Error
  }
