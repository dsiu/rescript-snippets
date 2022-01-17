// https://kevanstannard.github.io/rescript-blog/destructuring-types.html

type person = Person(string, int)

let joe = Person("Joe", 23)
let jim = Person("Jim", 31)

let Person(name1, age1) = joe
let Person(name2, age2) = jim

Js.log2(name1, age1)
Js.log2(name2, age2)

// "Joe" 23
// "Jim" 31
