%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI COLON SLASH
%token PLUS QUESTION
%token FUN RARROW TRUE FALSE INT BOOL SHIFT RESET IF THEN ELSE

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
  | FUN ID RARROW Expr { Fun (None, $2, None, $4) }
  | FUN LPAREN ID COLON Type RPAREN RARROW Expr { Fun (None, $3, Some $5, $8) }

PExpr :
  | PExpr PLUS AppExpr { BinOp (Plus, $1, $3) }
  | AppExpr { $1 }

AppExpr :
  | AppExpr AExpr { App ($1, $2) }
  | SRExpr { $1 }

SRExpr :
  | RESET LPAREN FUN LPAREN RPAREN RARROW Expr RPAREN { Reset ($7, None) }
  | SHIFT LPAREN FUN ID RARROW Expr RPAREN { Shift ($4, None, $6) }
  | SHIFT LPAREN FUN LPAREN ID COLON Type RPAREN RARROW Expr RPAREN { Shift ($5, Some $7, $10) }
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
  | FunType { $1 }

FunType :
  | AType SLASH AType RARROW AType SLASH AType  { TyFun ($1, $3, $5, $7) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | QUESTION { TyDyn }
