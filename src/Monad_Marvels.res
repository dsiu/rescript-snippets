//
// Marvels of functional programming: Composing effects with monads
//
// https://anil-thaplar.medium.com/marvels-of-functional-programming-composing-effects-with-monads-4972962d09ca
//

open Belt

type ironSuit = [#ASSEMBLED | #FUELED]
type ironMan = [#IRONMAN_SUITEDUP | #IRONMAN_ENGINE_STARTED]
type tonyStark = [#TonyStark]

module SuitUp = {
  let assembleParts = (): option<ironSuit> => {
    Some(#ASSEMBLED)
  }

  let fuel = (ironSuit: ironSuit): option<ironSuit> => {
    Some(#FUELED)
  }

  let suitUp = (ironSuit: ironSuit, stark: tonyStark): option<ironMan> => {
    Some(#IRONMAN_SUITEDUP)
  }
}

module EngineStart = {
  let preLaunchChecks = (ironMan: ironMan): option<unit> => {Some()}
  let startEngine = (ironMan: ironMan): option<ironMan> => {Some(#IRONMAN_ENGINE_STARTED)}
}

type coordinates = {x: int, y: int}
type launch = [#LAUNCHED]

module Launch = {
  let rangeVerification = (): option<coordinates> => {Some({x: 12, y: 14})}
  let launch = (ironMan: ironMan, coordinates: coordinates): option<launch> => {Some(#LAUNCHED)}
}

let tony = #TonyStark
let program =
  SuitUp.assembleParts()->Option.flatMap(suit =>
    suit
    ->SuitUp.fuel
    ->Option.flatMap(fueledSuit =>
      fueledSuit
      ->SuitUp.suitUp(tony)
      ->Option.flatMap(ironMan =>
        ironMan
        ->EngineStart.preLaunchChecks
        ->Option.flatMap(checkResult =>
          Launch.rangeVerification()->Option.flat Map(coord => ironMan->Launch.launch(coord))
        )
      )
    )
  )

program->Js.log
