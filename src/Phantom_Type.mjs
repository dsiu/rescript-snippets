// Generated by ReScript, PLEASE EDIT WITH CARE


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

var cantBePassed = "ok?";

var validatedData = shouldBeOkay;

export {
  Animal ,
  mike ,
  marla ,
  $$FormData ,
  shouldBeOkay ,
  validatedData ,
  cantBePassed ,
}
/*  Not a pure module */
