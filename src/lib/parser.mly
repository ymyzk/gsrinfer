%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI COLON SLASH CARET
%token PLUS MINUS QUESTION
%token FUN RARROW TRUE FALSE INT BOOL SHIFT RESET
%token IF THEN ELSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel

%%

toplevel :
  | Expr SEMISEMI { $1 }

Expr :
  | IfExpr { $1 }
  | FunExpr { $1 }
  | PExpr { $1 }

IfExpr :
  | IF Expr THEN Expr ELSE Expr { If ($2, $4, $6) }

FunExpr :
  | FUN OptionalAnswerTypeAnnot ID RARROW Expr { Fun ($2, $3, None, $5) }
  | FUN OptionalAnswerTypeAnnot LPAREN ID COLON Type RPAREN RARROW Expr { Fun ($2, $4, Some $6, $9) }

PExpr :
  | PExpr PLUS AppExpr { BinOp (Plus, $1, $3) }
  | PExpr MINUS AppExpr { BinOp (Minus, $1, $3) }
  | AppExpr { $1 }

AppExpr :
  | AppExpr SRExpr { App ($1, $2) }
  | SRExpr { $1 }

SRExpr :
  | RESET OptionalAnswerTypeAnnot SRExpr { Reset ($3, $2) }
  | SHIFT ID RARROW SRExpr { Shift ($2, None, $4) }
  | SHIFT LPAREN ID COLON Type RPAREN RARROW SRExpr { Shift ($3, Some $5, $8) }
  | AExpr { $1 }

AExpr :
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
