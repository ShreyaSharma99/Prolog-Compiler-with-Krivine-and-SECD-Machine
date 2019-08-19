#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
open A4;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_def df = match df with
  Simple(l,r) -> "def " ^ l ^ " = " ^ (print_tree r)
  | _ -> raise Not_implemented
;;


(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;

(* Sample parsing *)
print_endline ( print_tree (exp_parser "5" rho));;
print_endline ( print_def (def_parser "def A=5" rho));;

(* Sample test case *)
let e1 = (exp_parser "let def A = proj(1,3)Z in W(A) end" rho);;
let e5 = (exp_parser "\\X.Y" rho);;
let e = (exp_parser " (6,6,T /\\ X,X)" rho);;
let e8 = (exp_parser "\\X.(mod Y)" rho);;
let e9 = (exp_parser "\\X.(W(proj(3,3)Z))" rho )
let e7=(exp_parser "proj(2,3)(proj(1,2)((2,3,4),(4,5,X)))" rho);;
let e3 = (exp_parser "proj(1,3)((proj(2,2)((5,4),(2,3))),5,6)" rho);;
let e4 = (exp_parser "proj(1,4)((if ((if (5>3) then T else F fi) \\/ F) then 10 else (~3) fi), (T/\\F,F), (proj(2,2)(0,~20)),((1+3),(2+4),(3-6)))" rho);;
(* let e2 = (exp_parser  "proj(1,3)((proj(2,2)(((5+4), (if T then 6 else T fi),T),((T /\\ F),(4- 8), (6 mod 4)))),6,7)" rho);; *)
let e6 = (exp_parser  "let def V = \\X.\\Y.(if Y then X else X+1 fi) in V(2)(T) end" rho);;
let e2 = (exp_parser "let def Y = let def X = proj(1,3) ((1,2),2>3,30) in proj(1,2) X end in \\X.X(Y) end" rho);;
let t = Ttuple[Tint;Tint;Tbool;Tint];; 
(* let t=Tbool;; *)
(* Type assumptions as a list of tuples of the form (variable name, type) *)
let g = [("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];;
(* let d = (def_parser "def U = X ; (def V = Y ; def U = Y) ; def H = T" rho);; *)
let d = (def_parser "def U = X || def H = T ; def U = \\X.Y" rho);;

let g_dash = [("H", Tbool); ("U", Tfunc (Tint, Tbool))];;

assert(hastype g e t);;
assert(yields g d g_dash);;
