include Belt.Option

let getExnWithMessage = (option, message) => {
  switch option {
  | Some(value) => value
  | None => Js.Exn.raiseError(message)
  }
}

/**
  option(a,b): returns a if a is Some(_) other wise return b
 */
let optionOr: (option<'a>, option<'a>) => option<'a> = (a, b) => {
  switch a {
  | Some(_) => a
  | None => b
  }
}
