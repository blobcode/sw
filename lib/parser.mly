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
%token ASSIGN
%token SEMICOLON
%token COMMA
%token IF
%token TO
%token LENGTH
%token THEN
%token ELSE
%token MAP
%token BSLASH
%token ARROW
%token QUESTION
%token COLON
%token EOF

%nonassoc LOWPREC
%nonassoc ELSE
%left SEMICOLON
%left MAP
%nonassoc QUESTION COLON
%left LEQ
%left PLUS
%left TIMES
%nonassoc TO

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | e = app_expr { e }
  | e1 = expr; MAP; e2 = expr { InOp (Map, e1, e2) }
  | e1 = expr; TO; e2 = expr { InOp (Range, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { InOp (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { InOp (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { InOp (Add, e1, e2) }
  | e1 = expr; QUESTION; e2 = expr; COLON; e3 = expr { If (e1, e2, e3) }
  | x = ID; ASSIGN; e1 = expr; SEMICOLON; e2 = expr { Let (x, e1, e2) }
  | x = ID; ASSIGN; e = expr %prec LOWPREC { Let (x, e, Var x) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | BSLASH; params = nonempty_list(ID); ARROW; body = expr %prec LOWPREC 
      { Lambda (params, body) }
  ;

app_expr:
  | e = simple_expr { e }
  | e1 = app_expr; e2 = simple_expr { App (e1, e2) }
  | e = app_expr; LBRACKET; idx = expr; RBRACKET { Index (e, idx) }
  ;

simple_expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRACKET; es = separated_list(COMMA, expr); RBRACKET { List es }
  | LENGTH; LPAREN; e = expr; RPAREN { Length e }
  | LPAREN; e = expr; RPAREN { e }
  ;
