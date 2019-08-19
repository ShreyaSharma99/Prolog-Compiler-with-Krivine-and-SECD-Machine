(* abstract syntax *)
type expType = 
Tint 
| Tunit 
| Tbool ;;

type  expr =
 V of string 
 | Lambda of (expr * expType * expr) 
 | App of (expr * expr) 
 | Plus of (expr * expr) 
 | Mult of (expr * expr) 
 | And of (expr * expr) 
 | Or of (expr * expr) 
 | Bool of bool 
 | Integer of int 
 | Cmp of expr 
 | If_Then_Else of (expr * expr * expr)
 | Abs of expr                   (* abs *)
 | Negative of expr              (* unary minus ~ *)
 | Not of expr
 | Sub of expr * expr         (* Subtraction - *)
 | Div of expr * expr         (* div *)
 | Rem of expr * expr         (* mod *)
 | InParen of expr               (* ( ) *)
 
(* definition *)
type opcode = VAR of string | CONST of int | BCONST of bool | RET | PLUS | MULT | CONJ | DISJ | CMP | COND of opcode list * opcode list 
              | APP | CLOS of expr * opcode list | MINUS | DIV | REM | ABS | UNARYMINUS | NOT | PAREN ;;

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool 

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of int | Bool of bool | ValClosure of expr * opcode list * (string * answer) list 

type closure = Clos of expr * (string * closure) list | ValueClosure of  value * (string * closure) list 

val krivmc : closure ->  closure list -> closure 

val secd : answer list -> (string * answer) list -> opcode list -> (answer list * (string * answer) list * opcode list) list -> answer

(* the compiler *)
val compile : expr -> opcode list 

