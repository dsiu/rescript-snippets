//
// ref: https://fsharpforfunandprofit.com/posts/designing-with-types-single-case-dus/
//

module EmailAddress = {
  type t = EmailAddress(string)

  // create with continuation
  let createWithCont = (success, failure, s: string) => {
    switch Js.Re.exec_(%re("/^\S+@\S+\.\S+$/"), s) {
    | Some(_) => success(EmailAddress(s))
    | None => failure("Email address must contain an @ symbol")
    }
  }

  let create = s => {
    let success = e => Some(e)
    let failure = _ => None
    createWithCont(success, failure, s)
  }

  // unwrap with continuation
  let apply = (f, EmailAddress(e)) => f(e)

  // unwrap directly
  let value = e => apply(FP_Utils.id, e)
}

module ZipCode = {
  type t = ZipCode(string)

  // create with continuation
  let createWithCont = (success, failure, s: string) => {
    switch Js.Re.exec_(%re("/^\d{5}$/"), s) {
    | Some(_) => success(ZipCode(s))
    | None => failure("Zip code must ")
    }
  }

  // create directly
  let create = s => {
    let success = e => Some(e)
    let failure = _ => None
    createWithCont(success, failure, s)
  }

  // unwrap with continuation
  let apply = (f, ZipCode(e)) => f(e)

  // unwrap directly
  let value = e => apply(FP_Utils.id, e)
}

module StateCode = {
  type t = StateCode(string)

  // create with continuation
  let createWithCont = (success, failure, s: string) => {
    let s' = s->Js.String.toUpperCase
    let stateCodes = list{"AZ", "CA", "NY"}
    stateCodes->Belt.List.has(s', FP_Utils.eq)
      ? success(StateCode(s))
      : failure("State is not in list")
  }

  // create directly
  let create = s => {
    let success = e => Some(e)
    let failure = _ => None
    createWithCont(success, failure, s)
  }

  // unwrap with continuation
  let apply = (f, StateCode(e)) => f(e)

  // unwrap directly
  let value = e => apply(FP_Utils.id, e)
}

module PersonalName = {
  type t = {
    firstName: string,
    // use "option" to signal optionality
    middleInitial: option<string>,
    lastName: string,
  }
}

module EmailContactInfo = {
  type t = {
    emailAddress: string,
    isEmailVerified: bool,
  }
}

module PostalAddress = {
  type t = {
    address1: string,
    address2: string,
    city: string,
    state: string,
    zip: string,
  }
}

module PostalContactInfo = {
  type t = {
    address: PostalAddress.t,
    isAddressValid: bool,
  }
}

module Contact = {
  type t = {
    name: PersonalName.t,
    emailContactInfo: EmailContactInfo.t,
    postalContactInfo: PostalContactInfo.t,
  }
}

/*
Summary

To sum up the use of discriminated unions, here are some guidelines:

  * Do use single case discriminated unions to create types that represent the domain accurately.
  * If the wrapped value needs validation, then provide constructors that do the validation and enforce their use.
  * Be clear what happens when validation fails. In simple cases, return option types. In more complex cases, let the caller pass in handlers for success and failure.
  * If the wrapped value has many associated functions, consider moving it into its own module.
  * If you need to enforce encapsulation, use signature files.

*/
