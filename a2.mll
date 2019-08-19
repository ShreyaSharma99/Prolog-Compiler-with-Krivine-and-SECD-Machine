{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)
let digit0 = ['0'-'9']+
let whitespace = [' ' '\t' '\n']+
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let underscore = "_"
let apostrophe = "'"
let id = uppercase+(lowercase*uppercase*digit0*underscore*apostrophe*)*

(* This section is called the rules section. Think of a rule name as a
  function that, in this case, returns a value of type 'token'. Notice that
  everything inside the curly braces can be any arbitrary OCaml expression. *)

  (* ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF*)
rule read = parse       
eof                { EOF }
| whitespace {read lexbuf} 
| "~" {TILDA}
| "rem" {REM}
| ['0'-'9']+ as n  { INT (int_of_string n) }
| "T" {BOOL (true)}
| "F" {BOOL (false)}
| "abs" {ABS}
| "cmp" {CMP}
| "+" {PLUS}
| "-" {MINUS}                                          
| "*" {TIMES}
| "div" {DIV}
| "mod" {REM}
| "(" {LP}
| ")" {RP}
| "not" {NOT}
| "/\\" {CONJ}
| "\\/" {DISJ}
| "if" {IF}
| "fi" {FI}
| "then" {THEN}
| "else" {ELSE}
| "Tint" {TINT}
| "Tbool" {TBOOL}
| "Tunit" {TUNIT}
| ":"     {COLON}
| id as x {ID(x)}
| "\\"  {BACKSLASH}
| "."   {DOT}
| _                { raise Not_implemented }


     
(*DEF SEMICOLON PARALLEL LOCAL EOF*)