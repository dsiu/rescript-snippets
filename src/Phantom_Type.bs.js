// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function makeDog(a) {
  return a;
}

function makeCat(a) {
  return a;
}

function mate(a, b) {
  return a + " and " + b + " are now friends";
}

function interMate(a, b) {
  return a + " and " + b + " are now friends";
}

var Animal = {
  makeDog: makeDog,
  makeCat: makeCat,
  mate: mate,
  interMate: interMate
};

var mike = "Mike";

var marla = "Marla";

console.log(interMate(mike, marla));

function make(a) {
  return a;
}

function validate(a) {
  return a;
}

function saveToDB(a) {
  
}

var $$FormData = {
  make: make,
  validate: validate,
  saveToDB: saveToDB
};

var shouldBeOkay = "should be okay";

var validatedData = shouldBeOkay;

var cantBePassed = "ok?";

exports.Animal = Animal;
exports.mike = mike;
exports.marla = marla;
exports.$$FormData = $$FormData;
exports.shouldBeOkay = shouldBeOkay;
exports.validatedData = validatedData;
exports.cantBePassed = cantBePassed;
/*  Not a pure module */
