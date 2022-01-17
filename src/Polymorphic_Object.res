// ref:
// https://kevanstannard.github.io/rescript-blog/polymorphic-object.html

// The .. notation declares an open object type:
let logName = (o: {.."name": string}) => Js.log(o["name"])
let a = {"name": "Hello", "age": 20}
let b = {"name": "Cybertruck", "make": "Tesla"}
logName(a)
logName(b)
