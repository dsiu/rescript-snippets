@module external safeReadFile: string => Js.Promise.t<Js.Option.t<string>> = "./safeReadFile"
open Stdlib

safeReadFile("Result.res")
->Promise.then(x =>
  switch x {
  | None => "No cache, fetch data."
  | Some(data) => data
  }
  ->Js.log
  ->Promise.resolve
)
->ignore
