//
// How to handle JavaScript objects with different shapes in ReScript?
// ref:
// https://kevanstannard.github.io/rescript-blog/javascript-objects-different-shapes.html

// Sometimes we need to handle JavaScript data that may have arbitrary fields.
// Suppose we have the following data coming from JavaScript:

%%raw(`
const dataA = {
  body: "My hello content",
  attributes: {
    id: 1,
    title: "Hello",
    date: new Date("2020-12-14 19:47:57")
  }
}

const dataB = {
  body: "My world content",
  attributes: {
    title: "World",
    template: "main"
  }
}
`)

// Notice that the attributes are different in each case.
// How to type this data?
// If we know in advance what the possible shapes may be, we can use polymorphic object types.
// For example, we can type the data as follows:

@val external dataA: {"body": string, "attributes": {..}} = "dataA"
@val external dataB: {"body": string, "attributes": {..}} = "dataB"

// Notice the types of dataA and dataB are the same with the attributes type being an "open" object.
// Next, we can use inline types to dynamically assign the attribute types.
//
// For "dataA":
let handleDataA = (data: {"body": string, "attributes": {..}}) => {
  let o: {
    "body": string,
    "attributes": {"id": option<string>, "title": option<string>, "date": option<Js.Date.t>},
  } = data

  let body: string = o["body"]
  let id: option<string> = o["attributes"]["id"]
  let title: option<string> = o["attributes"]["title"]
  let date: option<Js.Date.t> = o["attributes"]["date"]

  Js.log(body)
  Js.log(id)
  Js.log(title)
  Js.log(date)
}

// And "dataB":
let handleDataB = (data: {"body": string, "attributes": {..}}) => {
  let o: {
    "body": string,
    "attributes": {"title": option<string>, "template": option<string>},
  } = data

  let body: string = o["body"]
  let title: option<string> = o["attributes"]["title"]
  let template: option<string> = o["attributes"]["template"]

  Js.log(body)
  Js.log(title)
  Js.log(template)
}

// Then test the types with:
handleDataA(dataA)
handleDataB(dataB)

// Notes
// This strategy is only useful when you know the data types. If the data types change, then ReScript won't know about it.
//
// Context
// The context for this post was parsing Markdown files containing "front matter".
//
// For the examples above, the Markdown file content was as follows.

// File #1:

//---
//id: 1
//title: Hello
//date: 2020-12-15 06:16:22
//---
//
//My hello content

// File #2:
//---
//title: World
//template: main
//---
//
//My world content
