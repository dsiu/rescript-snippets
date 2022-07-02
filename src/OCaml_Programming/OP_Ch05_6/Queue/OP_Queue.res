// // https://cs3110.github.io/textbook/chapters/modules/functional_data_structures.html

module type S = {
  @ocaml.doc(" An ['a t] is a queue whose elements have type ['a]. ")
  type t<'a>

  @ocaml.doc(" Raised if [front] or [dequeue] is applied to the empty queue. ") exception Empty

  @ocaml.doc(" [empty] is the empty queue. ")
  let empty: t<'a>

  @ocaml.doc(" [is_empty q] is whether [q] is empty. ")
  let is_empty: t<'a> => bool

  @ocaml.doc(" [enqueue x q] is the queue [q] with [x] added to the end. ")
  let enqueue: ('a, t<'a>) => t<'a>

  @ocaml.doc(" [front q] is the element at the front of the queue. Raises [Empty]
      if [q] is empty. ")
  let front: t<'a> => 'a

  @ocaml.doc(" [dequeue q] is the queue containing all the elements of [q] except the
      front of [q]. Raises [Empty] is [q] is empty. ")
  let dequeue: t<'a> => t<'a>

  @ocaml.doc(" [size q] is the number of elements in [q]. ")
  let size: t<'a> => int

  @ocaml.doc(" [to_list q] is a list containing the elements of [q] in order from
      front to back. ")
  let to_list: t<'a> => list<'a>
}
