@@warning("-32")

@val external describe: (string, @uncurry (unit => unit)) => unit = "describe"
@val external test: (string, @uncurry (unit => unit)) => unit = "test"
@val external _each1: (array<'a>, . string, 'a => unit) => unit = "test.each"
@val external _each2: (array<('a, 'b)>, . string, ('a, 'b) => unit) => unit = "test.each"
@val external _each3: (array<('a, 'b, 'c)>, . string, ('a, 'b, 'c) => unit) => unit = "test.each"
@val
external _each4: (array<('a, 'b, 'c, 'd)>, . string, ('a, 'b, 'c, 'd) => unit) => unit = "test.each"

// Helper methods that seem easier to use for me. Also automatically makes the
// title of the tests include the test index and parameters. Make sure to put ->
// ignore at the end of using these; if not sometimes there are warnings/errors
// in the javascript.
let testEach = (title, data, f) => _each1(data)(. `${title}(%#) %p`, f)
let testEach2 = (title, data, f) => _each2(data)(. `${title}(%#) %p %p`, f)
let testEach3 = (title, data, f) => _each3(data)(. `${title}(%#) %p %p %p`, f)
let testEach4 = (title, data, f) => _each4(data)(. `${title}(%#) %p %p %p %p`, f)

type m<'a> // matcher of type 'a
@val external expect: 'a => m<'a> = "expect"
@send external toBe: (m<'a>, 'a) => unit = "toBe"
@send external toEqual: (m<'a>, 'a) => unit = "toEqual"
@send external toThrow: m<'a> => unit = "toThrow"
@get external expectNot: m<'a> => m<'a> = "not"

// from https://github.com/snatvb/re-fp
@val external describe: (string, @uncurry (unit => unit)) => unit = "describe"
@val external test: (string, @uncurry (unit => unit)) => unit = "test"
type done = @uncurry (unit => unit)
type errorableDone<'a> = @uncurry (exn => Promise.t<'a>)
@val external testAsync: (string, @uncurry (done => unit)) => unit = "test"
external toErrorable: done => errorableDone<'a> = "%identity"

//type e
//@val external expect: 'a => e = "expect"
//@send external toBe: (e, 'a) => unit = "toBe"
//@send external toEqual: (e, 'a) => unit = "toEqual"

let awaitThen = (pa, done, f) =>
  pa
  ->Promise.thenResolve(a => {
    f(a)
    done()
  })
  ->Promise.catch(done->toErrorable)
  ->ignore
