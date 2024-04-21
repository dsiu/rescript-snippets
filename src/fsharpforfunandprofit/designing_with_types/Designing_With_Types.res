@@uncurried
@@uncurried.swap

type contact_orig = {
  firstName: string,
  middleInitial: string,
  lastName: string,
  emailAddress: string,
  //true if ownership of email address is confirmed
  isEmailVerified: bool,
  address1: string,
  address2: string,
  city: string,
  state: string,
  zip: string,
  //true if validated against address service
  isAddressValid: bool,
}

// Guideline: Use records or tuples to group together data that are required to be consistent (that is “atomic”) but don’t needlessly group together data that is not related.
type postalAddress = {
  address1: string,
  address2: string,
  city: string,
  state: string,
  zip: string,
}

type postalContactInfo = {
  address: postalAddress,
  isAddressValid: bool,
}

type personalName = {
  firstName: string,
  // use "option" to signal optionality
  middleInitial: option<string>,
  lastName: string,
}

type emailContactInfo = {
  emailAddress: string,
  isEmailVerified: bool,
}

type contact = {
  name: personalName,
  emailContactInfo: emailContactInfo,
  postalContactInfo: postalContactInfo,
}

// Wrapping primitive types
type emailAddress = EmailAddress(string)
type zipCode = ZipCode(string)
type stateCode = StateCode(string)

let createEmailAddress = (s: string) => {
  switch Js.Re.exec_(%re("/^\S+@\S+\.\S+$/"), s) {
  | Some(_) => Some(EmailAddress(s))
  | None => None
  }
}

let createStateCode = (s: string) => {
  let s' = s->Js.String2.toUpperCase
  let stateCodes = list{"AZ", "CA", "NY"} // etc
  switch stateCodes->Belt.List.has(s', (a, b) => {a == b}) {
  | true => Some(StateCode(s'))
  | _ => None
  }
}

createStateCode("ca")->Js.log
createStateCode("ba")->Js.log
createEmailAddress("someone@mail.com")->Js.log
createEmailAddress("diudiu")->Js.log

type creationResult<'a> = Success('a) | Error(string)

// Handling invalid input in a constructor
let createEmailAddress2 = (s: string) => {
  switch Js.Re.exec_(%re("/^\S+@\S+\.\S+$/"), s) {
  | Some(_) => Success(EmailAddress(s))
  | None => Error("Email address must contain an @ sign")
  }
}

createEmailAddress2("diudiu")->Js.log

// general approach uses continuations
let createEmailAddressWithContinuations = (success, failure, s: string) => {
  switch Js.Re.exec_(%re("/^\S+@\S+\.\S+$/"), s) {
  | Some(_) => success(EmailAddress(s))
  | None => failure("Email address must contain an @ sign")
  }
}

//let success = (EmailAddress(s)) => {
//  "success creating email: " ++ s
//}

//let failure = msg => {
//  "error creating email: " ++ msg
//}
let success = e => Some(e)
let failure = _ => failwith("bad email address")
createEmailAddressWithContinuations(success, failure, "someone@mail.com")->Js.log
//createEmailAddressWithContinuations(success, failure, "diudiu")->Js.log

let success = e => Some(e)
let failure = _ => None
createEmailAddressWithContinuations(success, failure, "someone@mail.com")->Js.log
//createEmailAddressWithContinuations(success, failure, "diudiu")->Js.log

// carry the success and failure
let createEmail = createEmailAddressWithContinuations(success, failure, _)
createEmailAddress("someone@mail.com")->Js.log2("with carry")
createEmailAddress("diudiu")->Js.log2("with carry")

// Creating modules for wrapper types
module EmailAddress = {
  type t = EmailAddress(string)

  let create = (s: string) => {
    switch Js.Re.exec_(%re("/^\S+@\S+\.\S+$/"), s) {
    | Some(_) => Some(EmailAddress(s))
    | None => None
    }
  }

  // unwrap with continuation
  let apply = (f, EmailAddress(e)) => f(e)

  // unwrap directly
  let value = (EmailAddress(e)) => e
}

let address1 = EmailAddress.create("x@example.com")

switch address1 {
| Some(EmailAddress(e)) => e->Js.log2("is a valid email address")
| None => ()
}
