
exception Invalid_Node of string
exception Invalid_Call 
exception Invalid of string 
exception Invalid_disp of string 
exception Its_parameter

type expr = 
Var of string
|Num of int
|Proc of string
|Ret
|Call of (expr * expr * expr)
|Assign of (expr * expr)

type var = ( string * int ref ) 

type node = Node of string * var list * string list * string | Node_ref of string * var list * string list * node ref

let main = Node("main", [("A",ref(0));("B",ref(0));("C",ref(0))], ["p";"q"], "" )
and
p = Node("p", [("X",ref(0));("Y",ref(0));("Z",ref(0));("A",ref(0))], ["r";"s"], "main" )
and
r = Node("r", [("W",ref(0));("I",ref(0));("J",ref(0));("B",ref(0))], ["v"], "p" )
and
v = Node("v", [("M",ref(0));("N",ref(0));("C",ref(0))], [], "r" )
and
s = Node("s", [("C",ref(0));("K",ref(0));("M",ref(0));("N",ref(0))], ["r"], "p" )
and
q = Node("q", [("Z",ref(0));("W",ref(0));("X",ref(0));("B",ref(0))], ["t";"u"], "main" )
and
t = Node("t", [("A",ref(0));("Y",ref(0));("I",ref(0));("F",ref(0))], ["w"], "q" )
and
w = Node("w", [("M",ref(0));("P",ref(0));("J",ref(0));("H",ref(0))], [], "t" )
and
u = Node("u", [("C",ref(0));("Z",ref(0));("P",ref(0));("G",ref(0))], [], "q" )
;;


let get_node st = match st with
"main" -> main
|"p" -> p
|"q" -> q
|"r" -> r
|"s" -> s
|"t" -> t
|"u" -> u
|"v" -> v
|"w" -> w
|_ -> raise(Invalid_Node("1")) 
;;

let rec nodelist c = match c with
x::xs -> (get_node x)::(nodelist xs)
|[] -> []
;;

let get_name n = match n with
Node(a,b,c,d) -> a
|Node_ref(a,b,c,d) -> a
|_ -> raise(Invalid_Node("2"))
;;

let get_varlist n = match n with
Node(a,b,c,d) -> b
|Node_ref(a,b,c,d) -> b
|_ -> raise(Invalid_Node("3"))
;;

let get_childlist n = match n with
Node(a,b,c,d) -> nodelist c
|Node_ref(a,b,c,d) -> nodelist c
|_ -> raise(Invalid_Node("4"))
;;
 (* note it returns parent string *)
 (* note that getparent is for string wala node only and not the ref wala node *)
let get_parent n = match n with
Node(a,b,c,d) -> d
|_ -> raise(Invalid_Node("5"))
;;

let get_parent_node n = match n with
Node_ref(a,b,c,d) -> !d
|_ -> raise(Invalid_Node("5"))
;;

let rec isMember a l =  match l with
[] -> false
| x::xs -> if (x=a) then true else isMember a xs
;;

let call_stack : node list = [main] ;;
let stack_pt = ref (call_stack);;
(* string of n1 node of stack calls n2 *)
let rec valid_call (n1:string) (n2:node) = if (isMember n2 (get_childlist (get_node n1))) then true else (match (get_node n1) with
																					Node(a,b,c,"") -> false;
																					|_ -> valid_call (get_parent (get_node n1)) n2 );;
 (* takes string of parent and the stack list as input and returns the node of latest parent called *)
let rec stack_parent str_parent c_stack = match c_stack with
x::xs -> if (get_name x) = str_parent then x else stack_parent str_parent xs
|[] -> raise (Invalid_Call);;

let create_node n a b = match n with
"r" ->   Node_ref("r", [("W",ref(a));("I",ref(b));("J",ref(0));("B",ref(0))], ["v"], ref(stack_parent "p" (!stack_pt)))
|"p" ->  Node_ref("p", [("X",ref(a));("Y",ref(b));("Z",ref(0));("A",ref(0))], ["r";"s"], ref(stack_parent "main" (!stack_pt)))
|"q" ->  Node_ref("q", [("Z",ref(a));("W",ref(b));("X",ref(0));("B",ref(0))], ["t";"u"],ref(stack_parent "main" (!stack_pt)))
|"s" ->  Node_ref("s", [("C",ref(a));("K",ref(b));("M",ref(0));("N",ref(0))], ["r"],  ref(stack_parent "p" (!stack_pt)))
|"t" ->  Node_ref("t", [("A",ref(a));("Y",ref(b));("I",ref(0));("F",ref(0))], ["w"], ref(stack_parent "q" (!stack_pt)))
|"u" ->  Node_ref("u", [("C",ref(a));("Z",ref(b));("P",ref(0));("G",ref(0))], [], ref(stack_parent "q" (!stack_pt)))
|"v" ->  Node_ref("v", [("M",ref(a));("N",ref(b));("C",ref(0))], [], ref(stack_parent "r" (!stack_pt)) )
|"w" ->  Node_ref("w", [("M",ref(a));("P",ref(b));("J",ref(0));("H",ref(0))], [], ref(stack_parent "t" (!stack_pt)) )
|_ ->    raise (Invalid_Call) ;;

(* node parameter1 parameter2 *)
let call n a1 b1 = if (valid_call (get_name(List.hd (!stack_pt))) (get_node n)) then stack_pt:=(create_node n a1 b1)::(!stack_pt) else raise (Invalid("here"));;

let rec display_stack stk = match stk with
x::xs -> (get_name x)::(display_stack xs)
|[] -> [];;

let rec find_remove (a,b) l lout = match l with
(s,t)::xs -> if (a=s) then lout@xs else (find_remove (a,b) xs (lout@[(s,t)]))
|[] -> lout;;

let rec seq_remove l1 l2 lout = match l1 with
(s,t)::xs ->  (seq_remove xs (find_remove (s,t) l2 lout) [])
|[] -> l2;;

let rec display_register n = match n with
x -> if (get_name x) <> "main" then let y=(get_parent_node x) in x::(display_register y) else [x]
|_ -> raise (Invalid_disp("11"));;

let rec var_access n = match (display_register n) with
x::xs -> if xs <> [] then (get_varlist x)@(seq_remove (get_varlist x) (var_access (List.hd xs)) []) else (get_varlist x)
|[] -> [];;

let rec var_change n = match (display_register n) with
x::xs -> if xs <> [] then (List.tl (List.tl(get_varlist x)))@(seq_remove (get_varlist x) (List.tl (List.tl(var_access (List.hd xs)))) []) else (get_varlist x)
|[] -> [];;

let rec update_var var_list v n = match  var_list with
(a,b)::xs -> if a=v then b:=n else update_var xs v n
|[] -> raise (Invalid("Variable not accessible"))
;;

let rec get_value_var var_list v = match var_list with
(a,b)::xs -> if a=v then (!b) else get_value_var xs v 
|[] -> raise (Invalid("Variable to be assigned is not accessible"))
;;

let assign var numb = update_var (var_change (List.hd !stack_pt)) var numb
;;

let assign_var var1 var2 = update_var (var_change (List.hd !stack_pt)) var1 (get_value_var (var_access (List.hd !stack_pt)) var2)
;;

let return r = stack_pt:= (List.tl !stack_pt);; 

let rec eval exp = match exp with
|Ret -> return 1
|Call(Proc(n),a1,b1) ->  (match (a1,b1) with
					(Var(s1),Var(s2)) ->  call n (get_value_var (var_access (List.hd !stack_pt)) s1)  (get_value_var (var_access (List.hd !stack_pt)) s2)
					|(Var(s1), Num(i)) -> call n  (get_value_var (var_access (List.hd !stack_pt)) s1) i
					|(Num(i), Var(s2)) -> call n  i (get_value_var (var_access (List.hd !stack_pt)) s2) 
					|(Num(i1), Num(i2)) -> call n i1 i2 
					)
|Assign(x,y) -> (match (x,y) with
				(Var(s),Num(i)) -> assign s i
				|(Var(s),Var(v)) -> assign_var s v
				)
;;

