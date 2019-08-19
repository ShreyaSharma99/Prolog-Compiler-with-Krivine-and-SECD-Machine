#directory "_build";; (* Consider this folder when looking for files *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A1;;
open A2;;
open A3;;
  
exception Not_implemented

let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;;

(* 
let p1 =  App (Lambda (V "x", Tint, Mult (Integer 3, V "x")), Integer 4);;
  (*12*)
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Tint, Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
   (*34*)
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Tint,Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let p4 = App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let p5 = App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)

let p6 = App(Lambda(V "x",Tint, Mult(V "x", App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)

let p7 = If_Then_Else(Cmp(App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Plus(V "x", V "y")), Integer 4)), Negative(Integer 5) )), Negative(Integer 29), App(Lambda(V "x",Tint, Plus(V "x", 
  App(Lambda(V "x",Tint, Plus(V "x", Integer 1)), Integer 7))), Integer 5));;  (*` for minus*)
(*13*)

let p8 = App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Plus(V "x", V "y")), Integer 4)), App(Lambda(V "x",Tint, Mult(V "x", Integer 2)), Integer 3));;
10

let p9 = App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Mult(V "x", V "y")), V "x")),Integer 4);;
(*16*)

let p10 = App(Lambda(V "x",Tint, Plus(V "x", App(Lambda(V "x",Tint, Mult(V "x", Integer 2)), App(Lambda(V "x",Tint, Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)

let p11 = App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, And(V "x", V "y")), V "x")), Bool true);;
(*true*)

let p12 = If_Then_Else(Cmp(App(Lambda(V "x",Tint, Mult(V "x", Integer 2)), Integer 4)), App(Lambda(V "x",Tint, App(Lambda(V "y",Tint, Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)

let p13 = App(Lambda(V "x",Tint, And(V "x", App(Lambda(V "x",Tint, And(V "x", Bool true)), App(Lambda(V "x",Tint, And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)

let p14 = App(Lambda(V "x",Tint, And(V "x", App(Lambda(V "x",Tint, And(V "x", Bool true)), App(Lambda(V "x",Tint, And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)

let p15 = If_Then_Else(Cmp(App(Lambda(V "x",Tint, Mult(V "x", App(Lambda(V "y",Tint, V "y"), V "x"))), Integer 1)), App(Lambda(V "x",Tint, Plus(V "x", App(Lambda(V "x", Tint, Plus(V "x", Integer 1)), Integer 3))), Integer 5), Negative(Integer 1));;(*9*)

 *)
let p1 = exp_parser "(\\X:Tint.(3*X))(4)";;
(* let p1 =  FunctionCall (FunctionAbstraction ( "x", Mult (N 3, Var "x")), N 4);; *)
  (*12*)
let p2 = exp_parser "if (cmp 7) then (\\X:Tint.(3+X))(31) else 0 fi";;
 (* let p2 = IfThenElse
   (GreaterT (N 7, N 0),
    FunctionCall (FunctionAbstraction ( "x", Add (N 3, Var "x")), N 31),
    N 0);;*)
   (*34*)

let p3 = exp_parser "if (cmp 0) then (\\X:Tint.(3+X))(4) else 110 fi";;
(* let p3 = IfThenElse
    (GreaterT(N 0, N 0),
    FunctionCall (FunctionAbstraction ( "x", Add (N 3, Var "x")), N 4),
        N 110);; *)
    (*110*)

let p4 = exp_parser "(\\X:Tint.((\\Y:Tint.(X /\\ Y))(T)))(F)";;
(* let p4 = FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", And(Var "x", Var "y")), B true)), B false);; *)
(*false*)

let p5 = exp_parser "(\\X:Tint.((\\Y:Tint.(X \\/ Y))(T)))(F)";;
(* let p5 = FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Or(Var "x", Var "y")), B true)), B false);; *)
(*true*)

let p6 = exp_parser "(\\X:Tint.(X * ((\\X:Tint.((\\Y:Tint.(X + Y))(4)))(3))))(2)";;
(* let p6 = FunctionCall(FunctionAbstraction( "x", Mult(Var "x", FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Add(Var "x", Var "y")), N 4)), N 3))), N 2);; *)
(*14*)

let p7 = exp_parser "if(cmp (\\X:Tint.((\\Y:Tint.(X + Y))(4)))(0-5)) then 0-29 else (\\X:Tint.(X + ((\\X:Tint.(X+1))(7))))(5) fi"
(* let p7 = IfThenElse(GreaterT(FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Add(Var "x", Var "y")), N 4)), N (-5)), N 0), N (-29), FunctionCall(FunctionAbstraction( "x", Add(Var "x", FunctionCall(FunctionAbstraction( "x", Add(Var "x", N 1)), N 7))), N 5));; *)
(*13*)

let p8 = exp_parser "(\\X:Tint.((\\Y:Tint.(X+Y))(4)))((\\X:Tint.(X*2))(3))";;
(* let p8 = FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Add(Var "x", Var "y")), N 4)), FunctionCall(FunctionAbstraction( "x", Mult(Var "x", N 2)), N 3));; *)
(*10*)

let p9 = exp_parser "(\\X:Tint.((\\Y:Tint.(X * Y))(X)))(4)";;
(* let p9 = FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Mult(Var "x", Var "y")), Var "x")), N 4);; *)
(*16*)

let p10 = exp_parser "(\\X:Tint.(X + ((\\X:Tint.(X*2))((\\X:Tint.(X+4))(3)))))(20)";;
(* let p10 = FunctionCall(FunctionAbstraction( "x", Add(Var "x", FunctionCall(FunctionAbstraction( "x", Mult(Var "x", N 2)), FunctionCall(FunctionAbstraction( "x", Add(Var "x", N 4)), N 3)))), N 20);; *)
(*34*)

let p11 = exp_parser "(\\X:Tint.((\\Y:Tint.(X /\\ Y))(X)))(T)";;
(* let p11 = FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", And(Var "x", Var "y")), Var "x")), B true);; *)
(*true*)

let p12 = exp_parser "if (cmp(\\X:Tint.(X*2))(4)) then (\\X:Tint.((\\Y:Tint.(X \\/ Y))(X)))(F) else T fi";;
(* let p12 = IfThenElse(GreaterT(FunctionCall(FunctionAbstraction( "x", Mult(Var "x", N 2)), N 4), N 0), FunctionCall(FunctionAbstraction( "x", FunctionCall(FunctionAbstraction( "y", Or(Var "x", Var "y")), Var "x")), B false), B true);; *)
(*false*)

let p13 = exp_parser "(\\X:Tint.(X /\\ ((\\X:Tint.(X /\\ T))((\\X:Tint.(X /\\ T))(T)))))(T)";;
(* let p13 = FunctionCall(FunctionAbstraction( "x", And(Var "x", FunctionCall(FunctionAbstraction( "x", And(Var "x", B true)), FunctionCall(FunctionAbstraction( "x", And(Var "x", B true)), B true)))), B true);; *)
(*true*)

let p14 = exp_parser "(\\X:Tint.(X /\\ ((\\X:Tint.(X /\\ T))((\\X:Tint.(X /\\ T))(T)))))(F)";;
(* let p14 = FunctionCall(FunctionAbstraction( "x", And(Var "x", FunctionCall(FunctionAbstraction( "x", And(Var "x", B true)), FunctionCall(FunctionAbstraction( "x", And(Var "x", B true)), B true)))), B false);; *)
(*false*)

let p15 = exp_parser "if (cmp (\\X:Tint.(X * ((\\Y:Tint.Y)(X))))(1)) then (\\X:Tint.(X + ((\\X:Tint.(X+1))(3))))(5) else 0-1 fi";;
(* let p15 = IfThenElse(GreaterT(FunctionCall(FunctionAbstraction( "x", Mult(Var "x", FunctionCall(FunctionAbstraction( "y", Var "y"), Var "x"))), N 1), N 0),
FunctionCall(FunctionAbstraction( "x", Add(Var "x", FunctionCall(FunctionAbstraction( "x", Add(Var "x", N 1)), N 3))), N 5), N (-1));; *)
(*9*)

(* let e1 = (exp_parser "let rec Fact =(\\N.(if N = 0 then 1 else N * Fact(N-1) fi)) in Fact(4) end" );;
 *)
(* let p16 = exp_parser "let def MultBy3 = \\X.(X*3) in MultBy3(4) end";; *)
(* 12 *)

let p17 = exp_parser "if cmp ~1 then cmp (2-(4+5)) else (((7+1)*6) div 4) - 32 fi";;
(* 126 *)

(* let p18 = exp_parser "let def R = (\\N.(N-1)) in R(4) end";; *)
(* 13 *)

(* let p19 = exp_parser "let def X = 3; def Y = X + 7; def Z = X*Y + 3 in ((\\X.\\Y:Tint.\\Z.(X + Y + Z))(1)(2)(3)) + Z end";; *)
(* 39 *)

(* let p20 = exp_parser "(\\X.3)(Z)";; *)
(* 3 in krivine, exception in secd *)
(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = secd [] [] (compile inp) [];;


(*Your code ends*)let eval_krivine inp = match (krivmc (Clos(inp,[])) [] ) with
                                          ValueClosure(NumVal(a),l) -> Integer a
                                          |ValueClosure(BoolVal(b),l)  -> Bool b ;;


let check_secd n inp out = let ans = eval_secd inp in]
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = eval_krivine inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 p1 (Num 12);;
check_secd 2 p2 (Num 34);;
check_secd 3 p3 (Num 110);;
check_secd 4 p4 (Bool false);;
check_secd 5 p5 (Bool true);;
check_secd 6 p6 (Num 14);;
check_secd 7 p7 (Num 13);;
check_secd 8 p8 (Num 10);;
check_secd 9 p9 (Num 16);;
check_secd 10 p10 (Num 34);;
check_secd 11 p11 (Bool true);;
check_secd 12 p12 (Bool false);;
check_secd 13 p13 (Bool true);;
check_secd 14 p14 (Bool false);;
check_secd 15 p15 (Num 9);; 

print_heading "Krivine test cases";;

check_krivine 1  p1 (Integer 12);;
check_krivine 2  p2 (Integer 34);;
check_krivine 3  p3 (Integer 110);;
check_krivine 4  p4 (Bool false);;
check_krivine 5  p5 (Bool true);;
check_krivine 6  p6 (Integer 14);;
check_krivine 7  p7 (Integer 13);;
check_krivine 8  p8 (Integer 10);;
check_krivine 9  p9 (Integer 16);;
check_krivine 10  p10 (Integer 34);;
check_krivine 11  p11 (Bool true);;
check_krivine 12  p12 (Bool false);;
check_krivine 13  p13 (Bool true);;
check_krivine 14  p14 (Bool false);;
check_krivine 15  p15 (Integer 9);; 
(*Krivine*)


