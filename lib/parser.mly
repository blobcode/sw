%{
open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token EQUALS
%token ASSIGN
%token SEMICOLON
%token COMMA
%token IF
%token TO
%token LENGTH
%token THEN
%token ELSE
%token EOF

%nonassoc ELSE
%left SEMICOLON
%left TO
%left LEQ
%left PLUS
%left TIMES
%nonassoc LBRACKET

%start <Ast.expr> prog
%%
prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; TO; e2 = expr { InOp (Range, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { InOp (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { InOp (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { InOp (Add, e1, e2) }
  | x = ID; ASSIGN; e1 = expr; SEMICOLON; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LBRACKET; es = separated_list(COMMA, expr); RBRACKET { List es }
  | e = expr; LBRACKET; idx = expr; RBRACKET { Index (e, idx) }
  | LENGTH; LPAREN; e = expr; RPAREN { Length e }
  | LPAREN; e=expr; RPAREN {e}
  ;
