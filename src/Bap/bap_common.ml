module Size = struct
  (** Defines possible sizes for operations operands  *)
  type all = [
    | `r8
    | `r16
    | `r32
    | `r64
    | `r128
    | `r256
  ] [@@deriving bin_io, compare, sexp, equal, variants]

  type 'a p = 'a constraint 'a = [< all]
  [@@deriving bin_io, compare, equal, sexp]

  type t = all p
  [@@deriving bin_io, compare, equal, sexp]


end

(** size of operand  *)
type size = Size.t
[@@deriving bin_io, compare, equal, sexp]

(** size of address  *)
type addr_size = [ `r32 | `r64 ] Size.p
[@@deriving bin_io, compare, equal, sexp]

type nat1 = int
[@@deriving bin_io, compare, equal, sexp]

(** The IR type of a BIL expression *)
module Type = struct
  type t =
    (** [Imm n] - n-bit immediate   *)
    | Imm of nat1
    (** [Mem (a,t)]memory with a specified addr_size *)
    | Mem of addr_size * size
    | Unk
end

type typ = Type.t


