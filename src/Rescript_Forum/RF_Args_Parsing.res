module R = RF_Result

type argType = String
type arg = {t: argType, name: string, minCount?: int, maxCount?: int}

let string = (name, ~required=false) => {
  t: String,
  name,
  minCount: required ? 1 : 0,
  maxCount: 1,
}

exception InvalidConfig(string)

let checkConfig = args => {
  // Keep track of names so we can detect duplicates.
  let argNames = Set.make()

  // Raises an exception if the specified arg has an invalid config.
  let checkArg = (arg): R.t<unit, 'a> => {
    if argNames->Set.has(arg.name) {
      raise(InvalidConfig(`${arg.name}: argument names must be unique`))
    }
    argNames->Set.add(arg.name)

    // We can just assume 0 and 1 for config sanity check purposes.
    let min = arg.minCount->Option.getOr(0)
    let max = arg.maxCount->Option.getOr(1)

    if min < 0 {
      raise(InvalidConfig(`${arg.name}: minCount cannot be negative`))
    }
    if max < 1 {
      raise(InvalidConfig(`${arg.name}: maxCount must be at least 1`))
    }
    if min > max {
      raise(InvalidConfig(`${arg.name}: minCount cannot exceed maxCount`))
    }

    Ok()
  }

  Ok(args)->R.forEach(checkArg)
}

type parseError = MissingParam(string)

let parseArgs = (argv, args) => {
  // Turn the arg vector into a stack which we can pop names/params from.
  let argv = argv->Array.toReversed

  let argMap = args->Array.map(arg => (arg.name, arg))->Dict.fromArray
  let argParams = Dict.make()

  let parseStringArg = (arg): R.t<JSON.t, parseError> =>
    switch argv->Array.pop {
    | Some(param) => Ok(param->JSON.Encode.string)
    | None => Err(MissingParam(`Argument '${arg.name}' requires exactly one parameter`))
    }

  let storeParam = (arg, param) =>
    switch argParams->Dict.get(arg.name) {
    | Some(JSON.Array(a)) => a->Array.push(param)
    | Some(p) => argParams->Dict.set(arg.name, [p, param]->JSON.Encode.array)
    | None => argParams->Dict.set(arg.name, param)
    }

  let parseArg = argv =>
    switch argv->Array.pop {
    | Some(name) =>
      let arg = argMap->Dict.getUnsafe(name)
      let param = switch arg.t {
      | String => parseStringArg(arg)
      }

      param->R.tap(storeParam(arg, _))->R.map(_ => Some(argv))
    | None => Ok(None)
    }

  Ok(argv)->R.repeat(parseArg)->R.map(_ => argParams)
}

let parse = (argv, args) => Ok(args)->R.andThen(checkConfig)->R.andThen(parseArgs(argv, _))
