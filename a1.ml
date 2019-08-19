(* Dummy implementation of A1 *)
(* open A0 *)
exception Invalid_Type
exception Invalid
exception Invalid_expr of string
exception Var_not_in_G


(* The possible types of expressions in the language of expressions *)
type expType = 
Tint 
| Tunit 
| Tbool ;;

(* abstract syntax *)
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
;;
 
(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | CONST of int | BCONST of bool | RET | PLUS | MULT | CONJ | DISJ | CMP | COND of opcode list * opcode list 
              | APP | CLOS of expr * opcode list | MINUS | DIV | REM | ABS | UNARYMINUS | NOT | PAREN ;;

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool ;;

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of int | Bool of bool | ValClosure of expr * opcode list * (string * answer) list ;;

type closure = Clos of expr * (string * closure) list | ValueClosure of  value * (string * closure) list ;;

let rec get_value s g =  match g with
[] -> raise(Var_not_in_G)
|(s1,v1)::xs -> if s=s1 then v1 else get_value s xs
;;

let rec find_remove (a,b) l lout = match l with
(s,t)::xs -> if (a=s) then lout@xs else (find_remove (a,b) xs (lout@[(s,t)]))
|[] -> lout;;

let rec seq_remove l1 l2 lout = match l1 with
(s,t)::xs ->  (seq_remove xs (find_remove (s,t) l2 lout) [])
|[] -> l2;;


(*make funcs get_type and get_value for variables ......................................*)
(* let rec krivmc cls ex stk : closure = match cls with
   Clos(Integer(v1),g1) -> (match ex with
                              []-> ValueClosure(NumVal(v1),g1)
                              |"Plus"::xs -> (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(NumVal(v2),g2) -> if xs=[] then ValueClosure(NumVal(v1+v2),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Integer(v1+v2),g1@(seq_remove g1 g2 []))) xs (List.tl stk) 
                                          |_ -> raise(Invalid_expr("1")))
                              |"Minus"::xs ->  (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(NumVal(v2),g2) -> if xs=[] then ValueClosure(NumVal(v2-v1),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Integer(v2-v1),g1@(seq_remove g1 g2 []))) xs (List.tl stk) 
                                          |_ -> raise(Invalid_expr("2")))
                              |"Mult"::xs ->  (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(NumVal(v2),g2) -> if xs=[] then ValueClosure(NumVal(v1*v2),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Integer(v1*v2),g1@(seq_remove g1 g2 []))) xs (List.tl stk)
                                          |_ -> raise(Invalid_expr("3")))
                              |"Div"::xs ->  (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(NumVal(v2),g2) -> if xs=[] then ValueClosure(NumVal(v2/v1),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Integer(v2/v1),g1@(seq_remove g1 g2 []))) xs (List.tl stk) 
                                          |_ -> raise(Invalid_expr("4")))
                              |"Rem"::xs ->  (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(NumVal(v2),g2) -> if xs=[] then ValueClosure(NumVal(v2 mod v1),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Integer(v2 mod v1),g1@(seq_remove g1 g2 []))) xs (List.tl stk)
                                          |_ -> raise(Invalid_expr("5")))
                              |s::xs ->  ValueClosure(NumVal(v1),g1)
                            )
   |Clos(Bool(v1),g1) -> (match ex with
                              []-> ValueClosure(BoolVal(v1),g1)
                              |"And"::xs -> (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(BoolVal(v2),g2) -> if xs=[] then ValueClosure(BoolVal(v1 && v2),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Bool(v1&&v2),g1@(seq_remove g1 g2 []))) xs (List.tl stk) 
                                          |_ -> raise(Invalid_expr("7")))
                              |"Or"::xs ->  (match (krivmc (List.hd stk) [] []) with
                                          ValueClosure(BoolVal(v2),g2) -> if xs=[] then ValueClosure(BoolVal(v1 || v2),g1@(seq_remove g1 g2 [])) else  krivmc (Clos(Bool(v1||v2),g1@(seq_remove g1 g2 []))) xs (List.tl stk) 
                                          |_ -> raise(Invalid_expr("8")))
                              |"If"::xs -> (if v1=true then krivmc (List.hd stk) [] [] else krivmc (List.hd (List.tl stk)) [] []  )
                              |s::xs ->  ValueClosure(BoolVal(v1),g1)
                            )
  |Clos(Plus(e1,e2),g) -> krivmc (Clos(e2,g)) ("Plus"::ex) ((Clos(e1,g))::stk) 
  |Clos(Sub(e1,e2),g) -> krivmc (Clos(e2,g)) ("Minus"::ex) ((Clos(e1,g))::stk)
  |Clos(Mult(e1,e2),g) -> krivmc (Clos(e2,g)) ("Mult"::ex) ((Clos(e1,g))::stk)  
  |Clos(Div(e1,e2),g) -> krivmc (Clos(e2,g)) ("Div"::ex) ((Clos(e1,g))::stk) 
  |Clos(Rem(e1,e2),g) -> krivmc (Clos(e2,g)) ("Rem"::ex) ((Clos(e1,g))::stk)  
  |Clos(And(e1,e2),g) -> krivmc (Clos(e2,g)) ("And"::ex) ((Clos(e1,g))::stk)  
  |Clos(Or(e1,e2),g) -> krivmc (Clos(e2,g)) ("Or"::ex) ((Clos(e1,g))::stk)  
  |Clos(Cmp(e1),g) ->   (match (krivmc (Clos(e1,g)) ex stk) with
                    ValueClosure(NumVal(v1),g1) -> if (v1>0) then (if ex=[] then ValueClosure(BoolVal(true),g1) else krivmc (Clos(Bool(true),g1)) ex stk) else (if ex=[] then ValueClosure(BoolVal(false),g1) else krivmc (Clos(Bool(false),g1)) ex stk)
                    |_ -> raise(Invalid_expr("10")) )
  |Clos(Abs(e1),g) ->   (match (krivmc (Clos(e1,g)) ex stk) with
                  ValueClosure(NumVal(v1),g1) -> if (v1>0) then (if ex=[] then ValueClosure(NumVal(v1),g1) else krivmc (Clos(Integer(v1),g1)) ex stk) else (if ex=[] then ValueClosure(NumVal(-v1),g1) else krivmc (Clos(Integer(-v1),g1)) ex stk)
                  |_ -> raise(Invalid_expr("11")) )
  |Clos(Negative(e1),g) ->   (match (krivmc (Clos(e1,g)) ex stk) with
                  ValueClosure(NumVal(v1),g1) -> if ex=[] then ValueClosure(NumVal(-v1),g1) else krivmc (Clos(Integer(-v1),g1)) ex stk
                  |_ -> raise(Invalid_expr("12")) )
  |Clos(Not(e1),g) ->   (match (krivmc (Clos(e1,g)) ex stk) with
                ValueClosure(BoolVal(v1),g1) -> if ex=[] then ValueClosure(BoolVal(not v1),g1) else krivmc (Clos(Bool(not v1),g1)) ex stk 
                |_ -> raise(Invalid_expr("13")) )
  |Clos(InParen(e1),g) ->   krivmc (Clos(e1,g)) ex stk
  |Clos(App(e1,e2),g) -> krivmc (Clos(e1,g)) ex ((Clos(e2,g))::stk) 
  |Clos(Lambda(V(x),t,e1),g) -> (match stk with
                          (Clos(e2,g1))::xs -> krivmc (Clos(e1,(x,(Clos(e2,g1)))::g)) ex xs 
                          | [] -> Clos(Lambda(V(x),t,e1),g) )
  |Clos(V(s),g) -> krivmc (get_value s g) ex []
  |Clos(If_Then_Else(e1,e2,e3),g) -> krivmc (Clos(e1,g)) ("If"::ex) ((Clos(e2,g))::(Clos(e3,g))::stk)  
  ;; *)


let rec compile (ex:expr) = match ex with
V(s) -> [VAR(s)]
|Integer(i) -> [CONST(i)]
|Bool(b) -> [BCONST(b)]
|Plus(e1,e2) -> (compile e1)@(compile e2)@[PLUS]
|Sub(e1,e2) -> (compile e1)@(compile e2)@[MINUS]
|Div(e1,e2) -> (compile e1)@(compile e2)@[DIV]
|Rem(e1,e2) -> (compile e1)@(compile e2)@[REM]
|Mult(t1,t2) -> (compile t1)@(compile t2)@[MULT]
|And(t1,t2) -> (compile t1)@(compile t2)@[CONJ]
|Or(t1,t2) -> (compile t1)@(compile t2)@[DISJ]
|Lambda(V(s),t,e2) ->  [CLOS(V(s), ((compile e2)@[RET]))]
|App(e1,e2) -> (compile e1)@(compile e2)@[APP]
|Cmp(e1) -> (compile e1)@[CMP]
|Abs(e1) -> (compile e1)@[ABS]
|Not(e1) -> (compile e1)@[NOT]
|Negative(e1) -> (compile e1)@[UNARYMINUS]
|InParen(e1) -> (compile e1)@[PAREN]
|If_Then_Else(e1,e2,e3) -> (compile e1)@[COND((compile e2),(compile e3))]
|_ -> raise(Invalid)
;;


let rec secd stk g c d = match c with
[] -> (match stk with
      s::[] -> s
      |_ -> raise(Invalid)) 
|CONST(i)::c1 -> secd ((Num(i))::stk) g c1 d
|BCONST(b)::c1 -> secd ((Bool(b))::stk) g c1 d
|VAR(s)::c1 -> secd ((get_value s g)::stk) g c1 d  (*............................................*)
|PLUS::c1 -> (match stk with
              Num(i1)::Num(i2)::xs -> secd ((Num(i1+i2))::xs) g c1 d 
              |_ -> raise(Invalid))
|MINUS::c1 -> (match stk with
              Num(i1)::Num(i2)::xs -> secd ((Num(i2-i1))::xs) g c1 d 
              |_ -> raise(Invalid))
|DIV::c1 -> (match stk with
              Num(i1)::Num(i2)::xs -> secd ((Num(i2/i1))::xs) g c1 d 
              |_ -> raise(Invalid))
|REM::c1 -> (match stk with
              Num(i1)::Num(i2)::xs -> secd ((Num(i2 mod i1))::xs) g c1 d 
              |_ -> raise(Invalid))
|MULT::c1 -> (match stk with
              Num(i1)::Num(i2)::xs -> secd ((Num(i1*i2))::xs) g c1 d 
              |_ -> raise(Invalid))  
|CONJ::c1 -> (match stk with
              Bool(i1)::Bool(i2)::xs -> secd (Bool(i1 && i2)::xs) g c1 d 
              |_ -> raise(Invalid)) 
|DISJ::c1 -> (match stk with
              Bool(i1)::Bool(i2)::xs -> secd (Bool(i1 || i2)::xs) g c1 d 
              |_ -> raise(Invalid)) 
|CMP::c1 ->  (match stk with
              Num(i)::xs -> if (i>0) then secd (Bool(true)::xs) g c1 d else secd (Bool(false)::xs) g c1 d )
|ABS::c1 ->  (match stk with
              Num(i)::xs -> if (i>0) then secd (Num(i)::xs) g c1 d else secd (Num(-i)::xs) g c1 d )
|UNARYMINUS::c1 ->  (match stk with
              Num(i)::xs -> secd (Num(-i)::xs) g c1 d )
|NOT::c1 ->  (match stk with
              Bool(b)::xs -> secd (Bool(not(b))::xs) g c1 d) 
|PAREN::c1 ->  secd stk g c1 d      
|CLOS(V(s),c2)::c1 -> secd ((ValClosure(V(s),c2,g))::stk) g c1 d
|APP::c1 ->  (match stk with
              a::ValClosure(V(x),c2,g1)::xs -> secd [] ((x,a)::g1) c2 ((xs,g,c1)::d) )
|RET::c1 ->  (match d with
              (s,g1,c2)::ds -> secd ((List.hd(stk))::s) g1 c2 ds
              |_ -> raise(Invalid) )
|COND(c2,c3)::c1 -> (match stk with 
          Bool(true)::xs ->  secd xs g (c2@c1) d
          | Bool(false)::xs ->  secd xs g (c3@c1) d
          |_ -> raise(Invalid))
|_ -> raise(Invalid)
;;


(* let p1 =  App (Lambda (V "x", Tint, Mult (Integer 3, V "x" )), Integer 4);;
let p1 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Tint, Sub (Integer 3, V "x")), Integer 31), 
    Integer 0);;
let p1 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Tint, Mult (Integer 3, V "x")), Integer 4),
        Integer 110);; 

         |Clos(Plus(e1,e2),g) -> (match ((krivmc (Clos(e1,g)) []),(krivmc (Clos(e2,g)) [])) with
                    (ValueClosure(NumVal(v1),g1),ValueClosure(NumVal(v2),g2)) -> ValueClosure(NumVal(v1+v2),g)
                    |_ -> raise(Invalid_expr) )



                    let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1+v2),g)
                                                    |_ -> raise(Invalid_expr) )

*)
let rec krivmc cls stk : closure = match cls with
  Clos(Integer(i),g) -> ValueClosure(NumVal(i),g)
  |Clos(Bool(b),g) -> ValueClosure(BoolVal(b),g)
  |Clos(Plus(e1,e2),g) -> let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1+v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Sub(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1-v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Mult(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1*v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Div(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1/v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Rem(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1 mod v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Mult(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(NumVal(v2),g2) -> ValueClosure(NumVal(v1*v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(And(e1,e2),g) ->let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(BoolVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(BoolVal(v2),g2) -> ValueClosure(BoolVal(v1 && v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Or(e1,e2),g) -> let stk = ((Clos(e2,g))::stk) in (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(BoolVal(v1),g1) -> (match (krivmc (List.hd stk) []) with
                                                    ValueClosure(BoolVal(v2),g2) -> ValueClosure(BoolVal(v1 || v2),g)
                                                    |_ -> raise(Invalid_expr("1")) ) )
  |Clos(Cmp(e1),g) ->   (match (krivmc (Clos(e1,g)) []) with
                    ValueClosure(NumVal(v1),g1) -> if (v1>0) then ValueClosure(BoolVal(true),g) else ValueClosure(BoolVal(false),g)
                    |_ -> raise(Invalid_expr("1")) )
  |Clos(Abs(e1),g) ->   (match (krivmc (Clos(e1,g)) []) with
                  ValueClosure(NumVal(v1),g1) -> if (v1>0) then ValueClosure(NumVal(v1),g) else ValueClosure(NumVal((-v1)),g)
                  |_ -> raise(Invalid_expr("1")) )
  |Clos(Negative(e1),g) ->   (match (krivmc (Clos(e1,g)) []) with
                  ValueClosure(NumVal(v1),g1) -> ValueClosure(NumVal(-v1),g) 
                  |_ -> raise(Invalid_expr("1")) )
  |Clos(Not(e1),g) ->   (match (krivmc (Clos(e1,g)) []) with
                ValueClosure(BoolVal(v1),g1) -> ValueClosure(BoolVal(not(v1)),g) 
                |_ -> raise(Invalid_expr("1")) )
  |Clos(InParen(e1),g) ->   krivmc (Clos(e1,g)) stk
  |Clos(App(e1,e2),g) -> krivmc (Clos(e1,g)) ((Clos(e2,g))::stk) 
  |Clos(Lambda(V(x),t,e1),g) -> (match stk with
                          (Clos(e2,g1))::xs -> krivmc (Clos(e1,(x,(Clos(e2,g1)))::g)) xs 
                          | [] -> Clos(Lambda(V(x),t,e1),g) )
  |Clos(V(s),g) -> krivmc (get_value s g) [] 
  |Clos(If_Then_Else(e1,e2,e3),g) -> if (krivmc (Clos(e1,g)) [])=ValueClosure(BoolVal(true),g) then (krivmc (Clos(e2,g)) []) else if (krivmc (Clos(e1,g)) [])=ValueClosure(BoolVal(false),g) then (krivmc (Clos(e3,g)) []) else raise(Invalid_expr("1"))
  ;;