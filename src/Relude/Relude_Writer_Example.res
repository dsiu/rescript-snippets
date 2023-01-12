module List = Relude.List
module WriterT = Relude.WriterT
module Writer = WriterT.Writer

let log = Js.log
let log2 = Js.log2

// create our own Writer with type t = string
module WriterLog = WriterT.WriterLog.List.WithEntry({
  type t = string
})

module WriterList = Writer.WithLog(WriterLog)

let map = WriterList.Infix.\"<$>"
let voidLeft = WriterList.Infix.\"$>"
let flipMap = WriterList.Infix.\"<$$>"
let bind = WriterList.Infix.\">>="

// "pure, >>=, tell, $>, runWriterT"
let writer = WriterList.pure(42)->bind(a => {
  Writer.tell(list{"a = " ++ string_of_int(a)})
  ->voidLeft(a * 2)
  ->bind(a => {
    Writer.tell(list{"a = " ++ string_of_int(a)})
    ->voidLeft(a + 5)
    ->bind({
      a => {
        Writer.tell(list{"a = " ++ string_of_int(a)})->voidLeft(a)
      }
    })
  })
})

let (a, writerLogs) = writer->Writer.runWriterT
a->log
writerLogs->Relude.Array.fromList->log
