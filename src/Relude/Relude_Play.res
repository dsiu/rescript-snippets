let log = Js.log
let log2 = Js.log2

type env = {
  intValue: int,
  stringValue: string,
}

let testEnv: env = {intValue: 42, stringValue: "abc"}

type error = {message: string}

module ReaderT = Relude.ReaderT

module Reader = Relude.Reader.WithEnv({
  type t = env
})

let r = Reader.make(r => r.intValue * 2) |> Reader.runReaderT(testEnv)
r->log
