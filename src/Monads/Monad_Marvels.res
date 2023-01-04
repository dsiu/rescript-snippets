//
// Marvels of functional programming: Composing effects with monads
//
// https://anil-thaplar.medium.com/marvels-of-functional-programming-composing-effects-with-monads-4972962d09ca
//

open Belt

type ironSuit = [#ASSEMBLED | #FUELED]
type ironMan = [#IRONMAN_SUITEDUP | #IRONMAN_PRELAUNCH_CHECKED | #IRONMAN_ENGINE_STARTED]
type tonyStark = [#TonyStark]

module SuitUp = {
  let assembleParts = (): option<ironSuit> => {
    Some(#ASSEMBLED)
  }

  let fuel = (ironSuit: ironSuit): option<ironSuit> => {
    ironSuit->ignore
    Some(#FUELED)
  }

  let suitUp = (ironSuit: ironSuit, stark: tonyStark): option<ironMan> => {
    ironSuit->ignore
    stark->ignore
    Some(#IRONMAN_SUITEDUP)
  }
}

module EngineStart = {
  let preLaunchChecks = (ironMan: ironMan): option<ironMan> => {
    ironMan->ignore
    Some(#IRONMAN_PRELAUNCH_CHECKED)
  }
  let startEngine = (ironMan: ironMan): option<ironMan> => {
    ironMan->ignore
    Some(#IRONMAN_ENGINE_STARTED)
  }
}

type coordinates = {x: int, y: int}
type launch = [#LAUNCHED]

module Launch = {
  let rangeVerification = (): option<coordinates> => {Some({x: 12, y: 14})}
  let launch = (ironMan: ironMan, coordinates: coordinates): option<launch> => {
    coordinates->ignore
    ironMan->ignore
    Some(#LAUNCHED)
  }
}

//program = Do
//assembleParts andThen
//           fuel andThen
//           suitUp andThen
//           preLauchChecks andThen
//           startEngine andThen
//           rangeVerfication andThen
//           launch

let program = {
  let tony = #TonyStark
  let tonySuitUp = SuitUp.suitUp(_, tony)
  let startedEngine =
    SuitUp.assembleParts()
    ->Option.flatMap(SuitUp.fuel)
    ->Option.flatMap(tonySuitUp)
    ->Option.flatMap(EngineStart.preLaunchChecks)
    ->Option.flatMap(EngineStart.startEngine)

  let launch = coords =>
    startedEngine->Option.flatMap(i => {
      i->Launch.launch(coords)
    })

  Launch.rangeVerification()->Option.flatMap(launch)
}

program->Js.log
