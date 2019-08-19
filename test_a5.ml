#directory "_build";; (* Consider this folder when looking for files *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A1;;
open A2;;
open A3;;

exception Not_implemented
(* Helper function to print *)

(* Input is given as value and output is an answer *)
(* let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
;;
 *)
(* Input is given as string and output is an answer *)

(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s = A3.exp_parser A2.read (Lexing.from_string s) 
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
;;

(* a-> ans of krivine machine and b-> ans of secd *)
let check a b = match (a,b) with
(ValueClosure(NumVal a1,l1), Num b1 ) -> if a1=b1 then true else false;
|_ -> true
;;

let p1 = parser ("3 mod 6 mod 5");;
let q1 = krivmc (Clos(p1,[])) [] [];;
let r1 = secd [] [] (compile p1) [];;
let s1 = check q1 r1 ;;

let p2 = parser ("if (cmp 6) then (\\X:Tint.(X + 6))(4) else (abs 9)+8 fi");;
let q2 = krivmc (Clos(p2,[])) [] [];;
let r2 = secd [] [] (compile p2) [];;
let s2 = check q2 r2 ;;

let p3 = parser ("\\X:Tbool.(X /\\ T) ");;
let q3 = krivmc (Clos(p3,[])) [] [];;
let r3 = secd [] [] (compile p3) [];;
let s3 = check q3 r3 ;;

let p1 = parser ("(\\X:Tint.(X+9))(3)  + (\\X:Tint.(X*2))(2) * (\\X:Tint.(X-5))(5)");;
let q1 = krivmc (Clos(p1,[])) [] [];;
let r1 = secd [] [] (compile p1) [];;
let s1 = check q1 r1 ;;

(*
"if (cmp (abs ~ 3)) then (\\X:Tint.(X mod 6))(4) else 0 fi"

let p1 = parser ("(\\X:Tint.(X+9))(3)  + (\\X:Tint.(X*2))(2) * (\\X:Tint.(X-5))(5)");;
let q1 = krivmc (Clos(p1,[])) [] [];;
let r1 = secd [] [] (compile p1) [];;
let s1 = check q1 r1 ;;
 *)



(* 
let p2 = parser( "5");;
let q2 = krivmc (Clos(p2,[])) []; *)

(* Input is given as string and output is a value *)



