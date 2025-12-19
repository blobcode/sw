{
open Parser
}

let white = [' ' '\t']+
let newline = '\n'
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | '"' ([^ '"']* as s) '"' { STR s }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET } 
  | ":=" { ASSIGN }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "." { PERIOD }
  | ".." { TO }
  | "len" { LENGTH }
  | "map" { MAP }
  | "@" { MAP }
  | "\\" { BSLASH }
  | "->" { ARROW }
  | "?" { QUESTION }
  | ":" { COLON }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
  
