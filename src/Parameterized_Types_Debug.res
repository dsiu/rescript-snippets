let saveThing = t => {
  t->Js.log
  Some(t)
}

let maybeSaveThing = (maybeThing: option<'a>): option<'a> => {
  switch maybeThing {
  | Some(thing) => saveThing(thing)
  | None => None
  }
}

maybeSaveThing(Some(5))->ignore
maybeSaveThing(Some("Siu"))->ignore
