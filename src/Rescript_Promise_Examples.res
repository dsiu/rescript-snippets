//
// https://github.com/ryyppy/rescript-promise
//
open RescriptCore
open Promise

//
// Creating a Promise:
//
let p1 = Promise.make((resolve, _reject) => {
  // We use uncurried functions for resolve / reject
  // for cleaner JS output without unintended curry calls
  resolve(. "hello world")
})
p1->Js.log

let p2 = Promise.resolve("some value")
p2->Js.log

// You can only reject `exn` values for streamlined catch handling
exception MyOwnError(string)
let p3 = Promise.reject(MyOwnError("some rejection"))
p3->Js.log

//
// Access and transform a promise value:
//
Promise.resolve("hello world")
->then(msg => {
  // then callbacks require the result to be resolved explicitly
  resolve("Message: " ++ msg)
})
->then(msg => {
  Js.log(msg)

  // Even if there is no result, we need to use resolve() to return a promise
  resolve()
})
->ignore // Requires ignoring due to unhandled return value

//
// Chain promises:
//

type user = {"name": string}
type comment = string

// mock function
let queryComments = (username: string): Js.Promise.t<array<comment>> => {
  switch username {
  | "patrick" => ["comment 1", "comment 2"]
  | _ => []
  }->resolve
}

// mock function
let queryUser = (_: string): Js.Promise.t<user> => {
  resolve({"name": "patrick"})
}

let queryUser =
  queryUser("u1")
  ->then(user => {
    // We use `then` to automatically
    // unnest our queryComments promise
    queryComments(user["name"])
  })
  ->then(comments => {
    // comments is now an array<comment>
    Belt.Array.forEach(comments, comment => Js.log(comment))

    // Output:
    // comment 1
    // comment 2

    resolve()
  })
  ->ignore

// You can also use thenResolve to chain a promise, and transform its nested value:
let createNumPromise = n => resolve(n)

createNumPromise(5)
->thenResolve(num => {
  num + 1
})
->thenResolve(num => {
  Js.log(num)
})
->ignore

//
// Catch promise errors:
//
// Important: catch needs to return the same return value as its previous then call
// (e.g. if you pass a promise of type Promise.t<int>, you need to return an int in your catch callback).
// This usually implies that you`ll need to use a result value to express successful / unsuccessful operations:
//
exception MyError(string)

Promise.reject(MyError("test"))
->then(str => {
  Js.log("this should not be reached: " ++ str)

  // Here we use the builtin `result` constructor `Ok`
  Ok("successful")->resolve
})
->catch(e => {
  let err = switch e {
  | MyError(str) => "found MyError: " ++ str
  | _ => "Some unknown error"
  }

  // Here we are using the same type (`t<result>`) as in the previous `then` call
  Error(err)->resolve
})
->then(result => {
  let msg = switch result {
  | Ok(str) => "Successful: " ++ str
  | Error(msg) => "Error: " ++ msg
  }
  Js.log(msg)
  resolve()
})
->ignore

// Catch promise errors caused by a thrown JS exception:
let causeErr = () => {
  Js.Exn.raiseError("Some JS error")->resolve
}

Promise.resolve()
->then(_ => {
  causeErr()
})
->catch(e => {
  let msg = switch e {
  | Exn.Error(obj) =>
    switch Exn.message(obj) {
    | Some(msg) => "JS exception occurred: " ++ msg
    | None => "Some other JS value has been thrown"
    }
  | _ => "Unexpected error occurred"
  }
  resolve(msg)
  // Outputs: Some JS error msg: Some JS error
})
->ignore

//
// Catch promise errors that can be caused by ReScript OR JS Errors (mixed error types):
//
// Every value passed to catch are unified into an exn value, no matter if those errors were thrown
// in JS, or in ReScript. This is similar to how we handle mixed JS / ReScript errors in
// synchronous try / catch blocks.
exception TestError(string)

let causeJsErr = () => {
  Js.Exn.raiseError("Some JS error")
}

let causeReScriptErr = () => {
  raise(TestError("Some ReScript error"))
}

// imaginary randomizer function
@val external generateRandomInt: unit => int = "generateRandomInt"

resolve()
->then(_ => {
  // We simulate a promise that either throws
  // a ReScript error, or JS error
  if generateRandomInt() > 5 {
    causeReScriptErr()
  } else {
    causeJsErr()
  }->resolve
})
->catch(e => {
  switch e {
  | TestError(msg) => Js.log("ReScript Error caught:" ++ msg)
  | Exn.Error(obj) =>
    switch Exn.message(obj) {
    | Some(msg) => Js.log("JS exception occurred: " ++ msg)
    | None => Js.log("Some other JS value has been thrown")
    }
  | _ => Js.log("Unexpected error occurred")
  }
  resolve()
})
->ignore

//
// Using a promise from JS (interop):
//
@val external someAsyncApi: unit => Js.Promise.t<string> = "someAsyncApi"

someAsyncApi()->Promise.then(str => Js.log(str)->resolve)->ignore

//
// Running multiple Promises concurrently:
//
let place = ref(0)

let delayedMsg = (ms, msg) => {
  Promise.make((resolve, _) => {
    Js.Global.setTimeout(() => {
      place := place.contents + 1
      resolve(. (place.contents, msg))
    }, ms)->ignore
  })
}

let p1 = delayedMsg(1000, "is Anna")
let p2 = delayedMsg(500, "myName")
let p3 = delayedMsg(100, "Hi")

all([p1, p2, p3])
->then(arr => {
  // arr = [ [ 3, 'is Anna' ], [ 2, 'myName' ], [ 1, 'Hi' ] ]

  Belt.Array.forEach(arr, ((place, name)) => {
    Js.log(`Place ${Belt.Int.toString(place)} => ${name}`)
  })
  // forEach output:
  // Place 3 => is Anna
  // Place 2 => myName
  // Place 1 => Hi

  resolve()
})
->ignore

//
// Race Promises:
//
let racer = (ms, name) => {
  Promise.make((resolve, _) => {
    Js.Global.setTimeout(() => {
      resolve(. name)
    }, ms)->ignore
  })
}

let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

race(promises)
->then(winner => {
  Js.log("Congrats: " ++ winner)->resolve
  // Congrats: Eagle
})
->ignore

//
// Common Mistakes
//

// Don't return a Promise.t<Promise.t<'a>> within a then callback:
resolve(1)
->then((value: int) => {
  let someOtherPromise = resolve(value + 2)

  // BAD: this will cause a Promise.t<Promise.t<'a>>
  resolve(someOtherPromise)
})
->then((p: Promise.t<int>) => {
  // p is marked as a Promise, but it's actually an int
  // so this code will fail
  p->then(n => Js.log(n)->resolve)
})
->catch(e => {
  Js.log("luckily, our mistake will be caught here")
  Js.log(e)
  // p.then is not a function
  resolve()
})
->ignore

// Don't return a Promise.t<'a> within a thenResolve callback:
resolve(1)
->thenResolve((value: int) => {
  // BAD: This will cause a Promise.t<Promise.t<'a>>
  resolve(value)
})
->thenResolve((p: Promise.t<int>) => {
  // p is marked as a Promise, but it's actually an int
  // so this code will fail
  p->thenResolve(n => Js.log(n))->ignore
})
->catch(e => {
  Js.log("luckily, our mistake will be caught here")
  // e: p.then is not a function
  e->ignore // or use e
  resolve()
})
->ignore

// ref:
// https://kevanstannard.github.io/rescript-blog/promise-empty-value.html
// Return empty value in a promise
let _ = Promise.make((resolve, _reject) => {
  resolve(. ignore())
})
