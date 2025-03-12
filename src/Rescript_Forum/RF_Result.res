
//
// Rescript frustration
//
// https://forum.rescript-lang.org/t/rescript-frustration/5983/44?u=dsiu
//


// Put `t` into a module so others can `open R.Types` to have it available
// without bringing all the functions into scope.
module Types = {
  type t<'a, 'b> = Ok('a) | Err('b)
}
include Types

let unit = (_: t<'a, 'b>): t<unit, 'b> => Ok()

let fromOptionOr = (opt, err) =>
  switch opt {
  | Some(x) => Ok(x)
  | None => Err(err)
  }

let fromOptionOrElse = (opt, fn) =>
  switch opt {
  | Some(x) => Ok(x)
  | None => Err(fn())
  }

let ok = res =>
  switch res {
  | Ok(x) => Some(x)
  | Err(_) => None
  }

let okExn = (res, ~message=?) =>
  switch res {
  | Ok(x) => x
  | Err(_) => panic(message->Option.getOr("R.okExn called for Err value"))
  }

let err = res =>
  switch res {
  | Ok(_) => None
  | Err(x) => Some(x)
  }

let errExn = (res, ~message=?) =>
  switch res {
  | Ok(_) => panic(message->Option.getOr("R.errExn called for Err value"))
  | Err(x) => x
  }

let getOr = (res, err) =>
  switch res {
  | Ok(x) => x
  | Err(_) => err
  }

let getOrElse = (res, fn) =>
  switch res {
  | Ok(x) => x
  | Err(_) => fn()
  }

let map = (res, fn) =>
  switch res {
  | Ok(x) => Ok(fn(x))
  | Err(x) => Err(x)
  }

let mapErr = (res, fn) =>
  switch res {
  | Ok(x) => Ok(x)
  | Err(x) => Err(fn(x))
  }

let andThen = (res, fn) =>
  switch res {
  | Ok(x) => fn(x)
  | Err(x) => Err(x)
  }

let orElse = (res, fn) =>
  switch res {
  | Ok(x) => Ok(x)
  | Err(x) => fn(x)
  }

let isOk = res =>
  switch res {
  | Ok(_) => true
  | Err(_) => false
  }

let isErr = res =>
  switch res {
  | Ok(_) => false
  | Err(_) => true
  }

let flatten = res =>
  switch res {
  | Ok(Ok(x)) => Ok(x)
  | Ok(Err(x)) => Err(x)
  | Err(x) => Err(x)
  }

let transpose = res =>
  switch res {
  | Ok(Some(x)) => Some(Ok(x))
  | Ok(None) => None
  | Err(x) => Some(Err(x))
  }

let tap = (res, fn) =>
  switch res {
  | Ok(x) =>
    fn(x)
    Ok(x)
  | Err(x) => Err(x)
  }

let rec repeat = (res, fn) =>
  switch res {
  | Ok(x) =>
    switch fn(x) {
    | Ok(Some(x)) => repeat(Ok(x), fn)
    | Ok(None) => res
    | Err(x) => Err(x)
    }
  | Err(x) => Err(x)
  }

let forEachWithIndex = (res: t<array<'a>, 'b>, fn) =>
  switch res {
  | Ok(a) =>
    let _ = Ok(0)->repeat(i => {
      switch a->Array.get(i) {
      | Some(v) => fn(v, i)
      | _ => Ok()
      }->map(() => i < a->Array.length ? Some(i + 1) : None)
    })
    Ok(a)
  | Err(x) => Err(x)
  }

let forEach = (res: t<array<'a>, 'b>, fn) => res->forEachWithIndex((x, _) => fn(x))
