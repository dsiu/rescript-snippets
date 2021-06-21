// https://forum.rescript-lang.org/t/how-to-create-a-js-json-t-from-a-record-without-libs-or-ppx/1873/5

external recordAsJson: 'a => Js.Dict.t<Js.Json.t> = "%identity"

type params = {id: int, name: string}
let params = {id: 1, name: "foo"}

let json = params->recordAsJson->Js.Json.object_
