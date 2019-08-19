open A1
exception Invalid of string
(* type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)
 *)

(*matches the variable 'str' with its type claimed 't' from the table 'g' starting from head to tail*)
let rec varType g str t = match g with
[] -> false
|x::xs -> (match x with
		(s,typ) -> if (str=s) then (if (t=typ) then true else false) else (varType xs str t)
		|_ -> raise (Invalid "g is invalid") );;

(*this function checks if an element is in a list*)
let rec isMember a l = match l with
[] -> false
|x::xs -> if a=x then true else (isMember a xs);;

(*takes exptree as input with type assumptions 'g' and returns the type of exp*)
let rec get_type g exp = match exp with
Var(str) -> (let rec varType g str = match g with
			[] -> Tunit
			|x::xs -> (match x with
					(s,typ) -> if (str=s) then typ else (varType xs str)
					|_ -> raise (Invalid "g is invalid") )
			in (varType g str))
|N(i) -> Tint 
|B(b) ->Tbool
|Abs(e1) -> if ((get_type g e1) = Tint) then Tint else Tunit
|Negative(e1) -> if (get_type g e1) = Tint then Tint else Tunit
|Not(e1) -> if (get_type g e1) = Tint then Tint else Tunit
|Add(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tint else Tunit
|Sub(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tint else Tunit
|Mult(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tint else Tunit
|Div(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tint else Tunit
|Rem(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tint else Tunit
|Conjunction(e1,e2) -> if ((get_type g e1) = Tbool) && ((get_type g e1) = Tbool) then Tbool else Tunit
|Disjunction(e1,e2) -> if ((get_type g e1) = Tbool) && ((get_type g e1) = Tbool) then Tbool else Tunit
|Equals(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tbool else Tunit
|GreaterTE(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tbool else Tunit
|GreaterT(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tbool else Tunit
|LessTE(e1,e2) -> if ((get_type g e1) = Tint) && (	(get_type g e1) = Tint) then Tbool else Tunit
|LessT(e1,e2) -> if ((get_type g e1) = Tint) && ((get_type g e1) = Tint) then Tbool else Tunit
|InParen(e1) -> get_type g e1		
|IfThenElse(e1,e2,e3) -> if ((get_type g e1)=Tbool)&&((get_type g e2)=(get_type g e3)) then (get_type g e3)  else Tunit
|Tuple(i,explist) ->(let rec give_list_type g explist = match explist with
											|x::xs -> (get_type g x)::(give_list_type g xs)
											|[] -> [] in Ttuple(give_list_type g explist))
|Project((i,j),e1) -> (match e1 with
 						Tuple(n,explist) -> (let rec get_type_i g i explist = match explist with
											x::xs -> if (i=1) then (get_type g x) else (get_type_i g (i-1) xs) 
											|[] -> raise (Invalid "inconsistent lists") in (get_type_i g i explist) )	 (*complete it for when e1 is not a tuple but a project*)
						|Var(v) ->(match (get_type g (Var(v))) with
									Ttuple(explist) -> (let rec get_type_i g i explist = match explist with
											x::xs -> if (i=1) then  (x) else (get_type_i g (i-1) xs) 
											|[] -> raise (Invalid "inconsistent lists") in (get_type_i g i explist) )
									|_ -> raise(Invalid "variable of wrong type") )
						|_ -> raise (Invalid "e not expected of this type") )
|FunctionAbstraction(s,e1) -> Tfunc((get_type g (Var(s))), (get_type g e1))
|Let(def,e1) -> (let rec def_typelist1 g def = match def with
				Simple(s,exp) ->  [(s, get_type g exp)]
				|Sequence(deflist) -> (match deflist with
										x::xs -> (def_typelist1 ((def_typelist1 g x)@g) (Sequence(xs)))@(def_typelist1 g x) 
										|[] -> [])
				|Parallel(deflist) -> (match deflist with
										x::xs -> (def_typelist1 g x)@(def_typelist1 g (Parallel(xs))) 
										|[] -> [])
				|Local(d1,d2) -> (def_typelist1 ((def_typelist1 g d1)@g) d2) in (get_type ((def_typelist1 g def)@g) e1))
|FunctionCall(e1,e2) -> (match (get_type g e1) with
						Tfunc(t1,t2) -> if ((get_type g e2)=t1 ) then t2 else Tunit
						|_ -> raise(Invalid "error"))
|_ -> raise(Invalid "not implemented");;

let rec isMember1 (a,b) l = match l with
[] -> false
|(a1,b1)::xs -> if a=a1 then true else (isMember1 (a,b) xs);;


let rec disjoint l1 l2 = match l1 with
(a,b)::xs -> if (isMember1 (a,b) l2) then false else (disjoint xs l2) 
| [] -> true ;;

 
let rec find_remove (a,b) l lout = match l with
(s,t)::xs -> if (a=s) then lout@xs else (find_remove (a,b) xs (lout@[(s,t)]))
|[] -> lout;;

let rec seq_remove l1 l2 lout = match l1 with
(s,t)::xs ->  (seq_remove xs (find_remove (s,t) l2 lout) [])
|[] -> l2;;

(*function to get a list of (string,exptype) from a definition*)
let rec def_typelist g def = match def with
Simple(s,exp) ->  [(s, get_type g exp)]
|Sequence(deflist) -> (match deflist with
						x::xs -> (def_typelist ((def_typelist g x)@g) (Sequence(xs)))@(seq_remove (def_typelist ((def_typelist g x)@g) (Sequence(xs))) (def_typelist g x) []) 
						|[] -> [])
|Parallel(deflist) -> (match deflist with
						x::xs -> if (disjoint (def_typelist g x) (def_typelist g (Parallel(xs)))) then (def_typelist g x)@(def_typelist g (Parallel(xs))) else raise(Invalid "parallel not disjoint")
						|[] -> [])
|Local(d1,d2) -> (def_typelist ((def_typelist g d1)@g) d2)
;;


(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with
Var(str) -> (varType g str t)
|N(i) -> t=Tint 
|B(b) -> t=Tbool
|Abs(e1) -> if t=Tint then (if (hastype g e1 Tint) then true else false ) else false
|Negative(e1) -> if t=Tint then (if (hastype g e1 Tint) then true else false ) else false
|Not(e1) -> if t=Tbool then (if (hastype g e1 Tbool) then true else false ) else false
|Add(e1,e2) -> if t=Tint then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|Sub(e1,e2) -> if t=Tint then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|Mult(e1,e2) -> if t=Tint then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false) else false
|Div(e1,e2) -> if t=Tint then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|Rem(e1,e2) -> if t=Tint then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|Conjunction(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tbool) && (hastype g e2 Tbool)) then true else false ) else false
|Disjunction(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tbool) && (hastype g e2 Tbool)) then true else false ) else false
|Equals(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|GreaterTE(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|GreaterT(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|LessTE(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|LessT(e1,e2) -> if t=Tbool then (if ((hastype g e1 Tint) && (hastype g e2 Tint)) then true else false ) else false
|InParen(e1) -> if (hastype g e1 t) then true else false			
|IfThenElse(e1,e2,e3) -> if (hastype g e1 Tbool)&&(hastype g e2 t)&&(hastype g e3 t) then true else false
|Tuple(i,explist) -> (match t with
					Ttuple(exptypelist) -> (let rec check_list_type g explist exptypelist = match (explist,exptypelist) with
											(x::xs,y::ys) -> if (hastype g x y) then (check_list_type g xs ys) else false
											|([],[]) -> true
											|_ -> raise (Invalid "inconsistent lists") in (check_list_type g explist exptypelist))
					|_ -> false ) 
|Project((i,j),e1) -> (match e1 with
 						Tuple(n,explist) -> (let rec check_type_i g i explist t = match explist with
											x::xs -> if i=1 then (hastype g x t) else check_type_i g (i-1) xs t
											|[] -> raise (Invalid "inconsistent lists") in (check_type_i g i explist t) )	 (*complete it for when e1 is not a tuple but a project*)
						|Var(v) -> (match (get_type g (Var(v))) with
									Ttuple(explist) -> (let rec check_type_i g i explist t = match explist with
											x::xs -> if i=1 then (x=t) else check_type_i g (i-1) xs t
											|[] -> raise (Invalid "inconsistent lists") in (check_type_i g i explist t) )
								) 
						(* (let rec get_type_var g v = match g with
									(x,t)::xs -> if(v=x) then t else (get_type_var xs v)
									|[] -> Tunit in  ) *)
						|_ -> raise (Invalid "e not expected of this type") )
|FunctionAbstraction(s,e1) -> (match t with
								Tfunc(t1,t2) -> if (hastype ([(s,t1)]@g) e1 t2) then true else false
								|_ -> raise(Invalid "e1 is not a function abstraction") )
|Let(def,e1) -> (hastype ((def_typelist g def)@g) e1 t)		(*g is appended later so that def_typelist is augmneted over g and each var is checked first in it and not in g*)
|FunctionCall(e1,e2) -> (match (get_type g e1) with
						Tfunc(t1,t2) -> if ((get_type g e2)=t1 )&&(t=t2) then true else false
						|_ -> raise(Invalid "error not a function FunctionAbstraction" ))
|_ ->  raise(Invalid "not implemented");;

(*function to check if one list is the subset of other*)
let rec sublist l1 l2 = match l1 with
x::xs -> if (isMember x l2) then (sublist xs l2) else false
|[] -> true;;

let rec yields g d g_dash = if (sublist (def_typelist g d) g_dash)&&(sublist g_dash (def_typelist g d)) then true else false
