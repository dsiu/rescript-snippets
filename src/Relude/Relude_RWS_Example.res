module RWST = Relude.RWST
module WriterT = Relude.WriterT

module Writer = WriterT.Writer

let log = Js.log
let log2 = Js.log2

module WriterLog = WriterT.WriterLog.List.WithEntry({
  type t = string
})

module WriterList = Writer.WithLog(WriterLog)

module RWST_WITHMONAD = RWST.WithMonad(Relude.Identity.Monad)

module RWS_M = RWST_WITHMONAD.WithEnvAndStateAndLog(
  {
    type t = int
  },
  {
    type t = int
  },
  WriterLog,
)

let computation = rws => {
  let e = rws->RWS_M
}

let example = RWS_M.runRWST(computation, 2, 3)
