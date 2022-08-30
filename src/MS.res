type options = {long: bool}
@module("ms") external fromMS: float => string = "default"
@module("ms") external fromMSWithOptions_: (float, options) => string = "default"
@module("ms")
external fromStr: string => float = "default"

let fromMSLong = s => {
  fromMSWithOptions_(s, {long: true})
}
