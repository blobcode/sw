open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let run (s : string) : string =
  let ast = parse s in
  let result = eval [] ast in
  string_of_val result
