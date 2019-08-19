#directory "_build";; (* Consider this folder when looking for files *)
#load "a3.cmo";;
#load "a6.cmo";;
#load "a2.cmo";;

open A3;;
open A6;;
open A2;;

exception Not_implemented

let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;;

let t1= "call p(2,3) ";;
let exp=exp_parser t1;;
eval exp;;

let t3= " A:=8 ";;
let exp3=exp_parser t3;;
eval exp3;;

let t3= "return";;
let exp3=exp_parser t3;;
eval exp3;;

(* let t2= "call q(5,6) ";;
let exp2=exp_parser t2;;
eval exp2;;

let t3= " X:=8 ";;
let exp3=exp_parser t3;;
eval exp3;;

let t3= " X:=Z ";;
let exp3=exp_parser t3;;
eval exp3;;
 *)
