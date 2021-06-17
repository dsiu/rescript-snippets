Js.log("======================================");
Js.log("First example, hard-coded using lists:");

type person = { age: int, name: string };

let countAdultsWithInitial = (initial, people) =>
  people
  |> List.filter(person => String.get(person.name, 0) == initial)
  |> List.filter(person => person.age >= 18)
  |> List.length

let people = [
  { age: 16, name: "Alice" },
  { age: 25, name: "Andrew" },
  { age: 34, name: "Ann" },
  { age: 22, name: "Bob" },
];

people
|> countAdultsWithInitial('A')
|> Js.log

Js.log("==============================================");
Js.log("Second example, refactoring into a transducer:");

module T = Transducer;

/* We introduce an infix operator to make it easier to compose long sequences
of function calls */
let (<<) = (f, g) => x => f(g(x));

let countAdultsWithInitial = initial =>
  (+) |> (
    T.filter(person => String.get(person.name, 0) == initial)
    << T.filter(person => person.age >= 18)
    << T.map(_ => 1)
  );

people
|> List.fold_left(countAdultsWithInitial('A'), 0)
|> Js.log

Js.log("=======================================================");
Js.log("Third example, reusing the pipeline for different ends:");

/* Ideally, we should be able to use point-free style with the function
composition operator, ommiting the `combine` function, but at the moment I'm
still struggling with ReasonML's type inference, and this was the only way I
could find to make it compile. I'll update the post accordingly as I find
better implementations. */
let adultsWithInitial = (initial, combine) =>
  combine |> (
    T.filter(person => String.get(person.name, 0) == initial)
    << T.filter(person => person.age >= 18)
  );

Js.log("Counting the selected records:");
let countAdultsWithInitial = initial =>
  (+) |> (
    adultsWithInitial(initial)
    << T.map(_ => 1)
  )

people
|> List.fold_left(countAdultsWithInitial('A'), 0)
|> Js.log

Js.log("Collecting the names of the selected records into a string:");
let join = separator => (result, element) => result ++ element ++ separator;
let enumerateAdultsWithInitial = initial =>
  join(", ") |> (
    adultsWithInitial(initial)
    << T.map(person => person.name)
  );

people
|> List.fold_left(enumerateAdultsWithInitial('A'), "")
|> Js.log

Js.log("Collecting the selected records into a list:");
let append = (list, element) => list @ [element];
people
|> List.fold_left(adultsWithInitial('A', append), [])
|> Js.log

Js.log("===========================================================================");
Js.log("Fourth and final example, using the transducer with another data structure:");

module Tree {
  type tree = Empty | Node(person, tree, tree);

  /* Reducing a tree in pre-order traversal */
  let rec reduce = (reducer, result, tree) => {
    switch tree {
      | Empty => result
      | Node(person, left, right) => {
        let resultSelf = reducer(result, person);
        let resultLeft = reduce(reducer, resultSelf, left);
        reduce(reducer, resultLeft, right);
      }
    };
  };
}

let people = Tree.(
  Node(
    { age: 34, name: "Ann" },
    Node(
      { age: 25, name: "Andrew" },
      Node(
        { age: 16, name: "Alice" },
        Empty,
        Empty,
      ),
      Empty,
    ),
    Node(
      { age: 22, name: "Bob" },
      Empty,
      Empty,
    )
  )
);

Js.log("Counting the selected records:");
people
|> Tree.reduce(countAdultsWithInitial('A'), 0)
|> Js.log

Js.log("Collecting the names of the selected records into a string:");
people
|> Tree.reduce(enumerateAdultsWithInitial('A'), "")
|> Js.log

Js.log("Collecting the selected records into a list:");
people
|> Tree.reduce(adultsWithInitial('A', append), [])
|> Js.log
