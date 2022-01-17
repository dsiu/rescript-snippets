//
// Reading JSON files in ReScript
// ref:
// https://kevanstannard.github.io/rescript-blog/read-json-file.html

// ReScript has built in support for reading JSON files.
//
// For example, to read the package.json file:

// Define the type and properties you want to access
// Alternatively you can use Js.Json.t
type package = {dependencies: Js.Dict.t<string>}

@module external package: package = "../package.json"

Js.Dict.keys(package.dependencies)->Js.Array2.forEach(Js.log)

ignore()
