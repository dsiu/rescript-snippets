open Belt

let makeStageA = (x): result<int, [> #ProblemA1 | #ProblemA2]> => {
  switch x {
  | 1 => Error(#ProblemA1)
  | 2 => Error(#ProblemA2)
  | x => Ok(x)
  }
}

let makeStageB = (x): result<int, [> #ProblemB1 | #ProblemB2]> => {
  switch x {
  | 1 => Error(#ProblemB1)
  | 2 => Error(#ProblemB2)
  | x => Ok(x)
  }
}

let makeStageC = (x): result<int, [> #ProblemC1 | #ProblemC2]> => {
  switch x {
  | 1 => Error(#ProblemC1)
  | 2 => Error(#ProblemC2)
  | x => Ok(x)
  }
}

let make = x => {
  let result = x->makeStageA->Result.flatMap(makeStageB)->Result.flatMap(makeStageC)

  // The brute force works but does not scale well with more variants and deepness
  /* let result = switch x->makeStageA {
  | Ok(x') =>
    switch x'->makeStageB {
    | Ok(x'') =>
      switch x''->makeStageC {
      | Ok(_) as ok => ok
      | Error(#ProblemC1) as err => err
      | Error(#ProblemC2) as err => err
      }
    | Error(#ProblemB1) as err => err
    | Error(#ProblemB2) as err => err
    }
  | Error(#ProblemA1) as err => err
  | Error(#ProblemA2) as err => err
  }*/

  switch result {
  | Ok(_x) => Js.log("Ok")
  | Error(#ProblemB1) => Js.log("Solve problem with foo")
  | Error(#ProblemA2) => Js.log("Solve problem with bar")
  | Error(_p) => Js.log("Do something other")
  }
}
