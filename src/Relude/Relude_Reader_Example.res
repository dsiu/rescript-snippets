module ReaderT = Relude.ReaderT

let log = Js.log
let log2 = Js.log2

type env = {
  version: int,
  product: string,
}

let testEnv = {version: 42, product: "relude"}

// create our own Reader with type t = env
module Reader = Relude.Reader.WithEnv({
  type t = env
})

// make
Reader.make(r => r.version * 2)->Reader.runReaderT(testEnv, _)->log
// -> 84

// ask
Reader.ask->Reader.map(a => a.version, _)->Reader.runReaderT(testEnv, _)->log
// -> 42

// asks
Reader.asks(r => r.version * 2)->Reader.map(a => a * 10, _)->Reader.runReaderT(testEnv, _)->log
// -> 840

// local
Reader.local(
  r => {version: r.version * 2, product: r.product ++ "!"},
  Reader.ask->Reader.map(a => string_of_int(a.version) ++ a.product, _),
)
->Reader.runReaderT(testEnv, _)
->log
// -> "84relude!"

// map
Reader.pure(42)->Reader.map(a => a * 2, _)->Reader.runReaderT(testEnv, _)->log
// -> 84

// apply
Reader.pure(42)
->Reader.apply(Reader.make((r, a) => a * r.version * 2), _)
->Reader.runReaderT(testEnv, _)
->log
// ->3528

// flatMap
Reader.pure(42)
->Reader.flatMap(a => Reader.make(r => r.version * a), _)
->Reader.runReaderT(testEnv, _)
->log
// -> 1764
