external _force_link_ : unit -> unit = "am_newsolver"

module Result = struct
  type t = Ok | Failed | Unsatisfied | Unbound
  let write = function
    | Ok -> 0
    | Failed -> -1
    | Unsatisfied -> -2
    | Unbound -> -3
  let read = function
    | 0 -> Ok
    | -1 -> Failed
    | -2 -> Unsatisfied
    | -3 -> Unbound
    | _ -> raise (Invalid_argument "Unexpected C enum")
  let typ = Ctypes.view ~read ~write Ctypes.int
end

module Relation = struct
  type t = Lessequal | Equal | Greatequal
  let write = function
    | Lessequal -> 1
    | Equal -> 2
    | Greatequal -> 3
  let read = function
    | 1 -> Lessequal
    | 2 -> Equal
    | 3 -> Greatequal
    | _ -> raise (Invalid_argument "Unexpected C enum")
  let typ = Ctypes.view ~read ~write Ctypes.int
end

module Strength = struct
  type t = Required | Strong | Medium | Weak
  let write = function
    | Required -> 1000000000.0
    | Strong -> 1000000.0
    | Medium -> 1000.0
    | Weak -> 1.0
  let read = function
    | 1000000000.0 -> Required
    | 1000000.0 -> Strong
    | 1000.0 -> Medium
    | 1.0 -> Weak
    | _ -> raise (Invalid_argument "Unexpected C enum")
  let typ = Ctypes.view ~read ~write Ctypes.double
end

open Ctypes
open Foreign

type solver = unit ptr
let solver : solver typ = ptr void
let newsolver = foreign "am_newsolver" (ptr void @-> ptr void @-> returning solver)

type variable = unit ptr
let variable : variable typ = ptr void
let newvariable = foreign "am_newvariable" (solver @-> returning variable)

let variableid = foreign "am_variableid" (variable @-> returning int)

type constraint_ = unit ptr
let constraint_ : constraint_ typ = ptr void
let newconstraint = foreign "am_newconstraint" (solver @-> Strength.typ @-> returning constraint_)

let addterm = foreign "am_addterm" (constraint_ @-> variable @-> double @-> returning Result.typ)

let setrelation = foreign "am_setrelation" (constraint_ @-> Relation.typ @-> returning Result.typ)

let add = foreign "am_add" (constraint_ @-> returning Result.typ)

let dumpsolver = foreign "am_dumpsolver" (solver @-> returning void)
