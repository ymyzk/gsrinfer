%{
open Syntax
%}

%token LPAREN RPAREN SEMI SEMISEMI COLON SLASH CARET
%token PLUS MINUS QUESTION
%token FUN RARROW TRUE FALSE INT BOOL SHIFT RESET
%token IF THEN ELSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel

%right RARROW
%right SEMI
%right prec_if
%left  PLUS MINUS
%right prec_app

%%

toplevel :
  | Expr SEMISEMI { $1 }

Expr :
  | Expr SEMI Expr { Consq ($1, $3) }
  | IF Expr THEN Expr ELSE Expr { If ($2, $4, $6) } %prec prec_if
  | FUN OptionalAnswerTypeAnnot ID RARROW Expr { Fun ($2, $3, None, $5) }
  | FUN OptionalAnswerTypeAnnot LPAREN ID COLON Type RPAREN RARROW Expr { Fun ($2, $4, Some $6, $9) }
  | Expr PLUS Expr { BinOp (Plus, $1, $3) }
  | Expr MINUS Expr { BinOp (Minus, $1, $3) }
  | SimpleExpr SimpleExpr { App ($1, $2) } (* %prec prec_app *)
  | RESET OptionalAnswerTypeAnnot Expr { Reset ($3, $2) } %prec prec_app
  | SHIFT ID RARROW Expr { Shift ($2, None, $4) } %prec prec_app
  | SHIFT LPAREN ID COLON Type RPAREN RARROW Expr { Shift ($3, Some $5, $8) } %prec prec_app
  | SimpleExpr { $1 }

SimpleExpr :
  | INTV { Const (ConstInt $1) }
  | TRUE { Const (ConstBool true) }
  | FALSE { Const (ConstBool false) }
  | LPAREN RPAREN { Const ConstUnit }
  | ID { Var $1 }
  | LPAREN Expr COLON COLON Type RPAREN { App (Fun (None, "x", Some $5, Var "x"), $2) }
  | LPAREN Expr RPAREN { $2 }

Type :
  | AType SLASH AType RARROW AType SLASH AType  { TyFun ($1, $3, $5, $7) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | QUESTION { TyDyn }

OptionalAnswerTypeAnnot :
  | { None }
  | CARET Type { Some $2 }
