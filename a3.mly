%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID 
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ LP RP IF THEN ELSE FI BACKSLASH DOT EOF COLON TINT TUNIT TBOOL CMP
%start exp_parser types_parser
%type <A1.expr> exp_parser /* Returns expression */
%type <A1.expType> types_parser
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
	or_expression EOF {$1}
;
or_expression:
	or_expression DISJ and_expression { Or($1,$3) }
	|and_expression { $1 }
;
and_expression:
	and_expression CONJ not_expression { And($1,$3) }
	|not_expression { $1 }
;
not_expression:
	NOT not_expression { Not($2) }
	|cmp_expression { $1 }
;
cmp_expression:      
	CMP addsub_expression { Cmp($2) }
	|addsub_expression { $1 }
;
addsub_expression:
	addsub_expression MINUS divmultrem_expression { Sub($1,$3) }
	|addsub_expression PLUS divmultrem_expression { Plus($1,$3) }
	|divmultrem_expression	{ $1 }
;
divmultrem_expression:
	divmultrem_expression REM abs_expression { Rem($1,$3)}	
	|divmultrem_expression TIMES abs_expression { Mult($1,$3) }
	|divmultrem_expression DIV abs_expression { Div($1,$3)}	
	|abs_expression	{ $1 }
;
abs_expression:
	ABS abs_expression { Abs($2) }
	|tilda_expression { $1 }
;
tilda_expression:
	TILDA tilda_expression { Negative($2) }
	|if_expression { $1 }
;
if_expression:
	IF or_expression THEN or_expression ELSE or_expression FI { If_Then_Else($2,$4,$6) }
	|fabstract_expression { $1 }	
;
fabstract_expression:
	BACKSLASH const COLON types_parser DOT or_expression {Lambda($2,$4,$6)}
	|fcall_expression					{ $1 }
;
fcall_expression:
	fabstract_expression LP or_expression RP {App($1,$3)}
	|const								{ $1 }						
;
const:
	ID                                 { V($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { Integer($1) }        /* To be interpreted as an integer with its value as tokenised   */
    | BOOL 							   { Bool($1) }
    |LP or_expression RP 			   { InParen($2) }
;
types_parser:
	TINT 								{ Tint }
	|TBOOL 								{ Tbool }
	|TUNIT 								{ Tunit }
;

/*proj always takes exp in parenthesis
let def Foo:Tint -> Tint = \\X.X in Foo(5) end
let e = exp_parser "let def Foo:Tint -> (Tint * Tbool) = \\X:Tint.(X,Y) in Foo(5) end" rho;;
*/
