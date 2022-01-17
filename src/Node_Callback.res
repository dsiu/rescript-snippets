//
// What is the type of a node callback in ReScript?
//
// ref:
// https://kevanstannard.github.io/rescript-blog/node-callback-type.html

// Node callbacks are typically of the form:
//function callback(error, items) {
//  if (error) {
//    // handle error
//  } else {
//    // handle items
//  }
//}

// The error argument
// The error argument may be null, or an error object. JavaScript errors in ReScript are typed as Js.Exn.t, so the error argument becomes:
type callbackError = Js.nullable<Js.Exn.t>

// The items argument
// The items argument may be null, or provide a value. We can use a generic type here for the value.
type callbackResult<'a> = Js.nullable<'a>

// The return value
// This function returns undefined in JavaScript, so the return value in ReScript will be unit;

// The callback function
// Now let's define a callback function type.
type callback<'a> = (. callbackError, callbackResult<'a>) => unit

// Note that node callbacks must be uncurried, so we use the (. ) function argument notation.
// If your callback only supplies an error, then you can use a similar type:
type callbackOnlyError = (. callbackError) => unit

// Utility function #1
// An example utility function for handling node callbacks that returns a Result.

let callbackWithResult = (
  f: Belt.Result.t<'a, Js.Exn.t> => unit,
  . error: callbackError,
  result: callbackResult<'a>,
) => {
  let errorOpt: option<Js.Exn.t> = Js.Nullable.toOption(error)
  let resultOpt: option<'a> = Js.Nullable.toOption(result)
  switch (errorOpt, resultOpt) {
  | (Some(error), _) => f(Belt.Result.Error(error))
  | (_, Some(result)) => f(Belt.Result.Ok(result))
  | (None, None) => raise(Invalid_argument("nodeCallback arguments invalid"))
  }
}

// Example usage:
@module("fs") external readFile: (string, string, callback<string>) => unit = "readFile"

let onResult = (result: result<string, Js.Exn.t>) => {
  let message = switch result {
  | Ok(result) => "Success: " ++ result
  | Error(error) => "Error: " ++ Belt.Option.getWithDefault(Js.Exn.message(error), "Unknown")
  }
  Js.log(message)
}

readFile("hello.txt", "UTF-8", callbackWithResult(onResult))

// Utility function #2
// Another example utility function that uses onSuccess and onError callbacks
let callbackWithSuccessOrError = (
  onSuccess: 'a => unit,
  onError: Js.Exn.t => unit,
  . error: callbackError,
  result: callbackResult<'a>,
) => {
  let errorOpt: option<Js.Exn.t> = Js.Nullable.toOption(error)
  let resultOpt: option<'a> = Js.Nullable.toOption(result)
  switch (errorOpt, resultOpt) {
  | (Some(error), _) => onError(error)
  | (_, Some(result)) => onSuccess(result)
  | (None, None) => raise(Invalid_argument("nodeCallback arguments invalid"))
  }
}

// Example usage:
@module("fs") external readFile: (string, string, callback<string>) => unit = "readFile"

let onSuccess = (result: string) => {
  let message = "Success: " ++ result
  Js.log(message)
}

let onError = (error: Js.Exn.t) => {
  let message = "Error: " ++ Belt.Option.getWithDefault(Js.Exn.message(error), "Unknown")
  Js.log(message)
}

readFile("hello.txt", "UTF-8", callbackWithSuccessOrError(onSuccess, onError))

// Utility function #3
// Last example converts the result to a promise.
let callbackWithPromise = (
  f: Js.Promise.t<'a> => unit,
  . error: callbackError,
  result: callbackResult<'a>,
) => {
  let errorOpt: option<Js.Exn.t> = Js.Nullable.toOption(error)
  let resultOpt: option<'a> = Js.Nullable.toOption(result)
  switch (errorOpt, resultOpt) {
  | (Some(error), _) => {
      let message = Belt.Option.getWithDefault(Js.Exn.message(error), "Unknown")
      f(Js.Promise.reject(Failure(message)))
    }
  | (_, Some(result)) => f(Js.Promise.resolve(result))
  | (None, None) => f(Js.Promise.reject(Failure("nodeCallback arguments invalid")))
  }
}

// Example usage:
@module("fs") external readFile: (string, string, callback<string>) => unit = "readFile"

let handlePromise = (promise: Js.Promise.t<string>) => {
  open Js.Promise
  promise
  ->then_((result: string) => resolve("Success: " ++ result), _)
  ->catch((_error: Js.Promise.error) => resolve("Error: Unknown"), _)
  ->then_(message => {
    Js.log(message)
    resolve()
  }, _)
  ->ignore
}

readFile("hello.txt", "UTF-8", callbackWithPromise(handlePromise))
