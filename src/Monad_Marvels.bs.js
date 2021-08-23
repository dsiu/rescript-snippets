// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Option = require("rescript/lib/js/belt_Option.js");

function assembleParts(param) {
  return "ASSEMBLED";
}

function fuel(ironSuit) {
  return "FUELED";
}

function suitUp(ironSuit, stark) {
  return "IRONMAN_SUITEDUP";
}

var SuitUp = {
  assembleParts: assembleParts,
  fuel: fuel,
  suitUp: suitUp
};

function preLaunchChecks(ironMan) {
  return "IRONMAN_PRELAUNCH_CHECKED";
}

function startEngine(ironMan) {
  return "IRONMAN_ENGINE_STARTED";
}

var EngineStart = {
  preLaunchChecks: preLaunchChecks,
  startEngine: startEngine
};

function rangeVerification(param) {
  return {
          x: 12,
          y: 14
        };
}

function launch(ironMan, coordinates) {
  return "LAUNCHED";
}

var Launch = {
  rangeVerification: rangeVerification,
  launch: launch
};

function tonySuitUp(__x) {
  return suitUp(__x, "TonyStark");
}

var startedEngine = Belt_Option.flatMap(Belt_Option.flatMap(Belt_Option.flatMap(Belt_Option.flatMap("ASSEMBLED", fuel), tonySuitUp), preLaunchChecks), startEngine);

function launch$1(coords) {
  return Belt_Option.flatMap(startedEngine, (function (i) {
                return launch(i, coords);
              }));
}

var program = Belt_Option.flatMap({
      x: 12,
      y: 14
    }, launch$1);

console.log(program);

exports.SuitUp = SuitUp;
exports.EngineStart = EngineStart;
exports.Launch = Launch;
exports.program = program;
/* startedEngine Not a pure module */
