%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI COLON SLASH
%token PLUS QUESTION
%token FUN RARROW TRUE FALSE INT BOOL SHIFT RESET

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel
%%

toplevel :
  | Expr SEMISEMI { $1 }

Expr :
  | SRExpr { $1 }
  | FunExpr { $1 }
  | PExpr { $1 }

SRExpr :
  | RESET LPAREN FUN LPAREN RPAREN RARROW Expr RPAREN { ResetI $7 }
  | SHIFT LPAREN FUN ID RARROW Expr RPAREN { ShiftI ($4, $6) }

PExpr :
  | PExpr PLUS AppExpr { BinOp (Plus, $1, $3) }
  | AppExpr { $1 }

AppExpr :
  | AppExpr AExpr { App ($1, $2) }
  | AExpr { $1 }

AExpr :
  | INTV { Const (ConstInt $1) }
  | TRUE { Const (ConstBool true) }
  | FALSE { Const (ConstBool false) }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

FunExpr :
  | FUN ID RARROW Expr { FunI ($2, $4) }
  | FUN LPAREN ID COLON Type RPAREN RARROW Expr { FunE ($3, $5, $8) }

Type :
  | FunType { $1 }

FunType :
  | AType SLASH FunType RARROW FunType SLASH FunType  { TyFun ($1, $3, $5, $7) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | QUESTION { TyDyn }
