// Monads and Gonads - Douglas Crockford
// ref:
// https://www.youtube.com/watch?v=b0EF0VTs9Dc&feature=youtu.be
//


function MONAD_0() {
	return function unit(value) {
		let monad = Object.create(null);
		monad.bind = function (func) {
			return func(value)
		};
		return monad
	}
}

// identity monad
let identity = MONAD_0()
let monad_0 = identity("Hello world.")
monad_0.bind(console.log)

// want to use any func on monad with more parameters
// expand the bind method
function MONAD_WITH_LIFT() {
  let prototype = Object.create(null)

  function unit(value) {
    let monad = Object.create(prototype)

    monad.bind = function (func, args) {
      return func(value, ...args)
    };
    return monad
  }

  unit.method = function (name, func) {
    prototype[name] = func;
    return unit
  }

  unit.lift = function (name, func) {
    prototype[name] = function (...args) {
      return unit(this.bind(func, args))
    }
    return unit
  }

  return unit
}

let ajax = MONAD_WITH_LIFT().lift('log', console.log)
let monad_1 = ajax("Hello world.")
monad_1.log()

//
// add Maybe
//
function MONAD_WITH_MOD (modifier) {
  let prototype = Object.create(null)

  function unit(value) {
    let monad = Object.create(prototype)

    monad.bind = function (func, args) {
      return func(value, ...args)
    }

    if (typeof modifier == 'function') {
      modifier(monad, value)
    }

    return monad;
  }

  return unit
}

// make a maybe monad
let maybe = MONAD_WITH_MOD(function (monad, value){
  if (value === null || value === undefined) {
    monad.is_null = true;
    monad.bind = function () {
      return monad
    }
  }
})

let monad_maybe = maybe(null)
monad_maybe.bind(console.log)

// let monad_maybe_1 = maybe("diu")
// monad_maybe_1.bind(console.log)
