// ref: https://kevanstannard.github.io/rescript-blog/reader-monad.html

open Test
open Monad_Reader
open Test_Utils

test("Reader Type", () => {
  Reader(e => e + 1)->ignore
})

// The run() function takes a reader and an environment passes the environment to the reader function.
test("run()", () => {
  //  let r1 = Reader(e => e + 1)
  //  let result = run(r1, 1)
  let result = Reader(e => e + 1)->run(1)
  intEqual(result, 2)
})

// The return() function creates a reader that always returns the same value, ignoring the environment.
test("return()", () => {
  //  let r2 = return(99)
  //  let result = run(r2, 1)
  let result = return(99)->run(1)
  intEqual(result, 99)
})

// The ask() function creates a reader that returns the environment variable.
test("ask()", () => {
  //  let r3 = ask()
  //  let result = run(r3, 123)
  let result = ask()->run(123)
  intEqual(result, 123)
})

// The local() function creates a reader that allows you to transform the environment value before passing it to a subsequent reader.
test("local()", () => {
  //  let r4 = Reader(e => e + 1)
  //  let r5 = local(e => -e, r4)
  //  let result = run(r5, 1)
  let result = Reader(e => e + 1)->local(e => -e, _)->run(1)
  intEqual(result, 0)
})

// The map() function creates a reader that applies a function to the result provided by the reader.
test("map()", () => {
  //  let r6 = Reader(e => e + 1)
  //  let r7 = map(x => x * 10, r6)
  //  let result = run(r7, 1)

  let result = Reader(e => e + 1)->map(x => x * 10, _)->run(1)
  intEqual(result, 20)
})

// The bind() function is similar to map, except that the applied function must return a reader rather than a value. This allows two readers to be bound together.
test("bind() 1", () => {
  //  let r8 = Reader(e => e + 1)
  //  let r9 = bind(x => Reader(_ => x * 2), r8)
  //  let result = run(r9, 1)
  let result = Reader(e => e + 1)->bind(x => Reader(_ => x * 2), _)->run(1)

  intEqual(result, 4)
})

test("bind() 2", () => {
  //  let r10 = Reader(e => e + 1)
  //  let r11 = bind(x => return(x * 2), r10)
  //  let result = run(r11, 1)

  let result = Reader(e => e + 1)->bind(x => return(x * 2), _)->run(1)
  intEqual(result, 4)
})

test("bind() 3", () => {
  let greet = (name, greeting) => greeting ++ ": " ++ name
  //  let lines = Array.map(Js.log)
  let ra = Reader(greet("One"))
  let rb = Reader(greet("Two"))
  let rc = Reader(greet("Three"))

  let r12 = bind(a => bind(b => bind(c => return([a, b, c]), rc), rb), ra)
  let result = run(r12, "Hello")
  stringArrayEqual(result, ["Hello: One", "Hello: Two", "Hello: Three"])
})

test("bind() 3.1", () => {
  let greet = (name, greeting) => greeting ++ ": " ++ name

  let result =
    Reader(greet("One"))
    ->bind(
      a =>
        Reader(greet("Two"))->bind(b => Reader(greet("Three"))->bind(c => return([a, b, c]), _), _),
      _,
    )
    ->run("Hey")
  stringArrayEqual(result, ["Hey: One", "Hey: Two", "Hey: Three"])
})
