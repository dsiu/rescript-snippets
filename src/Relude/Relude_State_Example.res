let log = Js.log
let log2 = Js.log2

module StateT = Relude.StateT

module State = StateT.State.WithState({
  type t = list<int>
})

//let ((<$>), ($>), (<$$>), ( *> ), (>>=)) =
//  State.Infix.((<$>), ($>), (<$$>), ( *> ), (>>=));

let map = State.Infix.\"<$>"
let voidLeft = State.Infix.\"$>"
let flipMap = State.Infix.\"<$$>"
let applySecond = State.Infix.\"*>"
let bind = State.Infix.\">>="

module Stack = {
  let push: int => StateT.State.t<int, list<int>> = x =>
    StateT.State.modify(xs => list{x, ...xs})->voidLeft(x)

  let pop: StateT.State.t<option<int>, list<int>> = State.get->bind(values =>
    switch values {
    | list{} => State.put(list{})->voidLeft(None)
    | list{x, ...xs} => State.put(xs)->voidLeft(Some(x))
    }
  )
}

// pure
let result = State.pure(2)->State.runStateT(list{}, _)
result->log2("pure", _)

//
// put
let result = State.pure(2)->bind(a => State.put(list{a})->voidLeft(a))->State.runStateT(list{}, _)
result->log2("put", _)

// stack example 1 (push)
let result =
  Stack.push(1)
  ->bind(_ => {
    Stack.push(2)
  })
  ->State.runStateT(list{}, _)
// (2, [2, 1])
result->log2("stack example 1 (push)", _)

// "stack example 2 (push, pop)"
let result =
  Stack.push(1)
  ->bind(_ => {
    Stack.push(2)->bind(_ => Stack.push(3)->bind({_ => Stack.pop}))
  })
  ->State.runStateT(list{}, _)

//(Some(3), [2, 1])
result->log2("stack example 2 (push, pop)", _)

// "stack example 3"
let result =
  Stack.push(1)
  ->bind(_ =>
    Stack.push(1)->bind(_ =>
      Stack.push(2)->bind(
        _ =>
          Stack.push(3)->bind(
            _ =>
              Stack.pop->bind(
                _ =>
                  Stack.pop->bind(
                    _ => Stack.pop->bind(_ => Stack.push(4)->bind(_ => Stack.push(5))),
                  ),
              ),
          ),
      )
    )
  )
  ->State.runStateT(list{}, _)
// (5, [5, 4, 1])
let (a, s) = result
a->log2("stack example 3", _)
s->log2("stack example 3", _)

// "stack example 4"
let result =
  Stack.push(1)
  ->bind(_ =>
    Stack.push(1)->bind(_ =>
      Stack.push(2)->bind(
        _ =>
          Stack.push(3)->bind(
            _ =>
              Stack.pop->bind(
                _ =>
                  Stack.pop->bind(
                    _ =>
                      Stack.pop->bind(
                        _ => Stack.push(4)->bind(_ => {Stack.push(5)->flipMap(a => a * 100)}),
                      ),
                  ),
              ),
          ),
      )
    )
  )
  ->State.runStateT(list{}, _)

// (500, [5, 4, 1])
let (a, s) = result
a->log2("stack example 4", _)
s->log2("stack example 4", _)

// "*> loses state"
let result =
  Stack.push(1)->applySecond(Stack.push(2))->applySecond(Stack.push(3))->State.runStateT(list{}, _)

// (3, [3])
let (a, s) = result
a->log2("*> loses state", _)
s->log2("*> loses state", _)
