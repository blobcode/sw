let rec repl () =
  print_string "> ";
  flush stdout;
  let line = input_line stdin in
  if String.trim line <> "" then (
    try
      let res = Sw.Main.run line in
      print_endline (res)
    with Failure msg -> print_endline ("Error: " ^ msg)
  );
  repl ()

let () = repl ()
