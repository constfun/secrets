external _force_link_ : unit -> unit = "am_newsolver"

open Ctypes
open Foreign

type solver = unit ptr
let solver : solver typ = ptr void
let newsolver = foreign "am_newsolver" (ptr void @-> ptr void @-> returning solver)

type variable = unit ptr
let variable : variable typ = ptr void
let newvariable = foreign "am_newvariable" (solver @-> returning variable)

let variableid = foreign "am_variableid" (variable @-> returning int)

let dumpsolver = foreign "am_dumpsolver" (solver @-> returning void)
