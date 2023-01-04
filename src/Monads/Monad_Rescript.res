// https://forum.rescript-lang.org/t/yet-another-proposal-for-monadic-computations/2595

//
// promise example
//
let produce = (a, b) => Js.Promise.all2((a, b))
let map = (p, f) => Js.Promise.then_(v => v->f->Js.Promise.resolve, p)

let concatContent = (c1, c2) => {
  switch (c1, c2) {
  | (Some(a), Some(b)) => Some(a ++ b)
  | (Some(a), None) => Some(a)
  | (None, Some(b)) => Some(b)
  | (None, None) => None
  }
}

let readAndConcatFiles = (f1, f2) => {
  f1
  ->produce(f2)
  ->map(((c1, c2)) => {
    concatContent(c1, c2)
  })
  ->map(c => {
    c->Js.log
  })
}

@module external safeReadFile: string => Js.Promise.t<Js.Option.t<string>> = "./safeReadFile"

readAndConcatFiles(safeReadFile("Color.res"), safeReadFile("Result.res"))->ignore

"========="->Js.log
//
// Result type
//

//switch (maybeCityId, maybeTargetUser, maybeBookingIds) {
//| (Ok(cityId), Ok(targetUser), Ok(bookingIds)) =>
//  Ok({
//    cityId: cityId,
//    targetUser: targetUser,
//    bookingIds: bookingIds,
//  })
//| (Error(_) as e, _, _) => e
//| (_, Error(_) as e, _) => e
//| (_, _, Error(_) as e) => e
//}

// vs
//for Belt.Result {
//  cityId <- maybeCityId
//  targetUser <- maybeTargetUser
//  bookingIds <- maybeBookingIds
//} yield {
//    cityId: cityId,
//    targetUser: targetUser,
//    bookingIds: bookingIds,
//}

module Result = {
  let product = (r1, r2) =>
    switch (r1, r2) {
    | (Ok(a), Ok(b)) => Ok((a, b))
    | (Error(e), _)
    | (_, Error(e)) =>
      Error(e)
    }
}

let maybeCityId = Ok(1)
let maybeTargetUser = Ok(2)
let maybeBookingIds = Ok(3)

type booking = {cityId: int, targetUser: int, bookingIds: int}

maybeCityId
->Result.product(maybeTargetUser)
->Result.product(maybeBookingIds)
->Belt.Result.map((((cityId, targetUser), bookingIds)) => {
  cityId: cityId,
  targetUser: targetUser,
  bookingIds: bookingIds,
})
->Belt.Result.map(t => t->Js.log)
->ignore

"========="->Js.log
