(* abstract syntax *)
type  expr =
Var of string
|Num of int
|Ret
|Call of (string * expr * expr)
|Assign of (expr * expr)

type var = ( string * int ref ) 

type node = Node of string * var list * string list * string | Node_ref of string * var list * string list * node ref

val eval : expr -> unit

val display_stack :  node list -> string list

val display_register :  node -> node list 

val var_access :  node -> var list


