open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let run (s : string) : string =
  let ast = parse s in
  let result = eval [] ast in
  string_of_val result

let rec extract_bindings env = function
  | Ast.Let (x, e_def, e_body) ->
      let v_def = Ast.eval env e_def in
      let new_env = (x, v_def) :: env in
      extract_bindings new_env e_body
  | _ -> env

let run_with_env env input =
  let lexbuf = Lexing.from_string input in
  let expr = Parser.prog Lexer.read lexbuf in
  let result = Ast.eval env expr in
  let new_env = extract_bindings env expr in
  (Ast.string_of_val result, new_env)
