(*open Core_kernel[@@warning "-D"]*)
(*open Regular.Std*)
(*open Format*)

open Bap_common
(*open Type*)

module T = struct
  type t = typ [@@deriving bin_io, compare, sexp]
  let module_name = Some "Bap.Std.Type"
  let version = "1.0.0"
  let hash = Hashtbl.hash
end
