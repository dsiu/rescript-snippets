open Belt

let log = Js.log

module Ch4_flatmap = {
  type customer = Customer(int)

  let getUser = id => {
    switch id {
    | 0 => None
    | x => Some(Customer(x))
    }
  }

  [0, 1, 2]->FP_Utils.flatMapArray(x => [getUser(x)])->log
}

module Ch4_Higher_Order_Functions = {
  type user = User({id: int, email: string})
  type error = Error({id: int, text: string})

  let getUsers = (): list<Result.t<user, error>> => {
    list{
      Ok(User({id: 1, email: "jack@example.com"})),
      Error(Error({id: 4, text: "user not found"})),
      Ok(User({id: 2, email: "andrea@example.com"})),
    }
  }

  let emails = getUsers()->List.map(result => result->Result.map((User(u)) => u.email))
  emails->List.toArray->log
}

module Ch4_Monads = {
  type student = Student({id: int, email: string})
  type finalGrade = FinalGrade({grade: int})

  let getStudent = (id: int): option<student> => {
    Some(Student({id: 1, email: "alex"}))
  }

  let getFinalGrade = (student: student): option<finalGrade> => {
    Some(FinalGrade({grade: 100}))
  }

  let student = getStudent(1)
  let
}
