// Regaining Control with State Monad and Friends (Felix Mulder)
// https://youtu.be/Pgo73GfHk0U
type seed = float

// random number gen (pseudo)
let rng_0 = (s: seed): (seed, float) => {
  let rand = Js.Math.random() -. 0.5
  (s +. rand, rand)
}

let rbg_0 = (s: seed): (seed, bool) => {
  let (newSeed, rand) = rng_0(s)
  (newSeed, rand > 0.0)
}

rng_0(3.0)->Js.log
rbg_0(2.0)->Js.log

// try adding 3 random numbers
let s0 = 2.0
let (s1, r0) = rng_0(s0)
let (s2, r1) = rng_0(s1)
let (_, r2) = rng_0(s2)
(r0 +. r1 +. r2)->Js.log

type state<'s, 'a> = State('s, 'a)

let rng = (s: seed): state<seed, float> => {
  let rand = Js.Math.random() -. 0.5
  State(s +. rand, rand)
}

let rbg = (s: seed): state<seed, bool> => {
  let State(newSeed, rand) = rng(s)
  State(newSeed, rand > 0.0)
}

let nextLong = seed => rng(seed)
let nextBool = seed => rbg(seed)

nextLong(1.0)->Js.log
nextLong(2.0)->Js.log
nextLong(3.0)->Js.log
nextBool(1.0)->Js.log
nextBool(2.0)->Js.log
nextBool(3.0)->Js.log
// run<'s, 'a> = 's => ('s, 'a)

//let nextLong:

"Use re-fp (not working, should use Relude)"->Js.log
//module R = REFP__Reader

//let getSeed = R.from(0.14)
//let double = n => n *. 2.0
//let addOne = n => n +. 1.0
//let f = getSeed->R.map(double)->R.map(addOne)->R.map(addOne)
//0->f->Js.log
