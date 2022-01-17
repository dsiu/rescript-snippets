//
// How to use Unicode characters in ReScript?

// ref:
// https://kevanstannard.github.io/rescript-blog/unicode-characters.html

//Instead use backtick quoted strings.

//Example, double quotes:
Js.log("😀")
// Prints something like "ð"

//Example, backticks:
Js.log(`😀`)
// Prints 😀
